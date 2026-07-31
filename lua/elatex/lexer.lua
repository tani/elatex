-- elatex.lexer -- UTF-8-aware source scanning and preprocessing
-- SPDX-License-Identifier: GPL-3.0-or-later

local data = require("elatex.data")
local strings = require("elatex.string")
local Text = require("elatex.text")
local M = {max_string_bytes = 100000}

local function char(point) return Text.from_codepoints({point}):to_string() end
local function text_at(text, begin, ending) return text:view(begin, ending):to_string() end
local function whitespace(point) return point == 32 or point == 9 or point == 10 or point == 13 or point == 12 or point == 11 end
local function token(begin, font)
  return {args={}, nargs=0, opt={}, nopt=0, sub=nil, super=nil, next=begin,
          self=begin, limits=0, p=data.pd_none, f=font}
end
local function set_args(item, values) item.args=values; item.nargs=#values end
local function set_options(item, values) item.opt=values; item.nopt=#values end

function M.ascii_letter(point) return point and ((point >= 65 and point <= 90) or (point >= 97 and point <= 122)) end
local function command_kind(point)
  if M.ascii_letter(point) then return 1 end
  if point == 44 or point == 59 or point == 58 or point == 92 or point == 34 then return 2 end
  return 0
end
function M.command_end(text, begin)
  if begin == nil or begin >= text:length() then return nil end
  local ending=begin+1
  if command_kind(text:char_at(ending)) == 2 then return ending+1 end
  while ending < text:length() and command_kind(text:char_at(ending)) == 1 do ending=ending+1 end
  return ending
end
function M.command(text, begin)
  local ending=M.command_end(text,begin)
  if not ending then return nil,begin end
  local name=text_at(text,begin,ending)
  if #name > 1 and M.ascii_letter(text:char_at(ending-1)) and text:char_at(ending) == 32 then ending=ending+1 end
  return name,ending
end
local function lookup_record(text, begin, index)
  local ending=M.command_end(text,begin)
  return ending and index[text_at(text,begin,ending)] or nil
end
function M.lookup_key(text, begin) return lookup_record(text,begin,data.key_index) end
function M.lookup_environment(name) return data.environment_index[name] end
local function symbol_end(text, begin)
  local at=begin+1
  if at < text:length() and (text:char_at(at)==44 or text:char_at(at)==59) then return at+1 end
  if at < text:length() then at=at+1 end
  while at < text:length() and M.ascii_letter(text:char_at(at)) do at=at+1 end
  return at
end
function M.lookup_symbol(text, begin) return data.symbol_index[text_at(text,begin,symbol_end(text,begin))] end
function M.lookup_font(name, context)
  local record=data.key_index["\\"..name]; local identity=record and record[2]
  local valid={ [data.pd_text]=true,[data.pd_mathbf]=true,[data.pd_mathbfit]=true,[data.pd_mathcal]=true,[data.pd_mathscr]=true,[data.pd_mathfrak]=true,[data.pd_mathbb]=true,[data.pd_mathsf]=true,[data.pd_mathsfbf]=true,[data.pd_mathsfit]=true,[data.pd_mathsfbfit]=true,[data.pd_mathtt]=true,[data.pd_mathnormal]=true }
  if valid[identity] then return identity end
  context.errors:add(13); return data.pd_text
end
function M.lookup_delimiter(value, begin)
  local text=type(value)=="string" and Text.new(value) or value
  for i=1,#data.delimiters do
    local record=data.delimiters[i]; local name=record[1]; local length=Text.new(name):length()
    if begin+length <= text:length() and text_at(text,begin,begin+length)==name then return record[2],name end
  end
  return data.del_none,"."
end
function M.lookup_combining(identity) return data.combining_command_index[identity] or {identity,0,0,0} end

local function balanced(text, begin, open, close, context)
  local length=text:length()
  if begin >= length or text:char_at(begin) ~= open then return nil,begin end
  local depth,ending=1,begin
  while ending < length and depth > 0 do
    ending=ending+1
    if ending < length then
      local point=text:char_at(ending)
      if point==open then depth=depth+1 elseif point==close then depth=depth-1 end
  end
  end
  if depth > 0 then context.errors:add(12) end
  return text_at(text,begin+1,ending), ending < length and ending+1 or ending
end
function M.option(text, begin, context) return balanced(text,begin,91,93,context) end
function M.argument(text, begin, context)
  while begin < text:length() and whitespace(text:char_at(begin)) do begin=begin+1 end
  local value,next_at=balanced(text,begin,123,125,context)
  if value ~= nil then return value,next_at end
  local point=text:char_at(begin)
  if point == 92 then return M.command(text,begin) end
  if not point or ("\\ ^_+-*/()@#$%&,{};\n"):find(char(point),1,true) then return nil,begin end
  return char(point),begin+1
end
function M.script(text, begin, context)
  local length=text:length()
  if begin >= length then return "",begin end
  if text:char_at(begin) ~= 92 and text:char_at(begin) ~= 123 then return char(text:char_at(begin)),begin+1 end
  if text:char_at(begin) == 123 then return balanced(text,begin,123,125,context) end
  local ending=begin+1
  while ending < length and not (" \t+-*/&\\_^}"):find(char(text:char_at(ending)),1,true) do ending=ending+1 end
  if ending < length and (text:char_at(ending)==32 or text:char_at(ending)==125) then ending=ending+1 end
  return text_at(text,begin,ending),ending
end

local function peek(item,text,begin,context)
  local backup,length,reset=begin,text:length(),true
  while begin < length and text:char_at(begin)==32 do begin=begin+1 end
  local record=M.lookup_key(text,begin); local identity=record and record[2]
  if identity==data.pd_limits then begin=begin+Text.new(record[1]):length(); reset=false; item.limits=1
  elseif identity==data.pd_nolimits then begin=begin+Text.new(record[1]):length(); reset=false; item.limits=0
  elseif identity==data.pd_over or identity==data.pd_choose then
    reset=false; item.p=identity==data.pd_over and data.pd_frac or data.pd_binom
    local left=text_at(text,item.self,begin); begin=begin+(identity==data.pd_over and 5 or 7)
    while begin<length and (text:char_at(begin)==32 or text:char_at(begin)==9) do begin=begin+1 end
    local right,next_at
    if begin<length and text:char_at(begin)==123 then right,next_at=M.argument(text,begin,context)
    elseif begin>=length or text:char_at(begin)==92 then right,next_at=nil,begin
    else
      local ending=begin
      while ending<length and not ("\\ \t{"):find(char(text:char_at(ending)),1,true) do ending=ending+1 end
      right=ending>begin and text_at(text,begin,ending) or nil; next_at=ending
    end
    if right==nil then context.errors:add(28); item.p=data.pd_none; item.next=begin
    else set_args(item,{left,right}); return peek(item,text,next_at,context) end
    begin=nil
  end
  if begin then
    while begin<length and (text:char_at(begin)==95 or text:char_at(begin)==94) do
      reset=false; local kind=text:char_at(begin); local value,next_at=M.script(text,begin+1,context)
      if kind==95 then if item.sub then context.errors:add(14) end; item.sub=value else if item.super then context.errors:add(15) end; item.super=value end
      begin=next_at
    end
    item.next=reset and backup or begin
  end
  return item
end

local function left_middle_right(text,begin,context)
  local length,depth,arg1,arg2,middle=text:length(),1,nil,nil,nil
  while begin<length and text:char_at(begin)==32 do begin=begin+1 end
  local open_id,open=M.lookup_delimiter(text,begin)
  if open_id==data.del_none then context.errors:add(16) else begin=begin+Text.new(open):length() end
  local ending=begin
  while ending<length and depth>0 do
    if text:char_at(ending)==92 then
      local tail=text_at(text,ending,math.min(length,ending+7))
      if tail:sub(1,6)=="\\right" then depth=depth-1
      elseif tail:sub(1,5)=="\\left" then depth=depth+1
      elseif depth==1 and tail=="\\middle" then
        arg1=text_at(text,begin,ending); ending=ending+7
        while ending<length and text:char_at(ending)==32 do ending=ending+1 end
        local middle_id; middle_id,middle=M.lookup_delimiter(text,ending)
        if middle_id==data.del_none then context.errors:add(16) else begin=ending+Text.new(middle):length() end
      end
    end
    ending=ending+1
  end
  ending=ending-1
  if ending>=0 and ending<length then
    arg2=text_at(text,begin,ending); ending=ending+6
    while ending<length and text:char_at(ending)==32 do ending=ending+1 end
    local close_id,close=M.lookup_delimiter(text,ending)
    if close_id==data.del_none then context.errors:add(16) else ending=ending+Text.new(close):length() end
    return {ending,arg1 or "",arg2 or "",open,middle or ".",close}
  end
  context.errors:add(17); return {length,arg1 or "","",open,middle or ".","."}
end
M.left_middle_right=left_middle_right

local function matching_environment_end(text,begin,context)
  local at,depth=text and begin or 0,0
  while at<text:length() do
    local rest=text_at(text,at,math.min(text:length(),at+6))
    if rest=="\\begin" then depth=depth+1; at=at+6
    elseif text_at(text,at,math.min(text:length(),at+4))=="\\end" then
      depth=depth-1; at=at+4; local _,next_at=M.argument(text,at,context); at=next_at
      if depth==0 then return at end
    else at=at+1 end
  end
  return at
end

local function table_read(text,begin,context)
  local cells,current,at,column,expected,row,hsep,line={""},"",begin,0,nil,0,{"c"},false
  local function set_current() cells[#cells]=current end
  local function set_sep(index,value) while #hsep<index+1 do hsep[#hsep+1]="c" end; hsep[index+1]=value end
  while at<text:length() do
    local six=text_at(text,at,math.min(text:length(),at+6)); local four=text_at(text,at,math.min(text:length(),at+4))
    if six=="\\begin" then local ending=matching_environment_end(text,at,context); current=current..text_at(text,at,ending);at=ending;line=true
    elseif four=="\\end" then at=at+4;break
    elseif six=="\\hline" then
      if column==0 then if row>0 and hsep[row]=="-" then context.errors:add(18);row=row-1 end;set_sep(row,"-");row=row+1;set_sep(row,"c")
      elseif not expected or column==expected then expected=expected or column;row=row+1;set_sep(row,"-") else context.errors:add(19) end
      at=at+6
    elseif six:sub(1,5)=="\\left" then local ending=at+5; -- retain a balanced left/right block as cell text
      local depth=1; while ending<text:length() and depth>0 do if text_at(text,ending,math.min(text:length(),ending+6))=="\\right" then depth=depth-1 elseif text_at(text,ending,math.min(text:length(),ending+5))=="\\left" then depth=depth+1 end;ending=ending+1 end; current=current..text_at(text,at,ending);at=ending;line=true
    elseif text:char_at(at)==38 then set_current();cells[#cells+1]="";current="";column=column+1;at=at+1;line=true
    elseif text:char_at(at)==92 and text:char_at(at+1)==92 then
      set_current();at=at+2;row=row+1;line=false;set_sep(row,"c");expected=expected or column
      if column~=expected then if expected<column then for _=1,column-expected do cells[#cells+1]=nil end else context.errors:add(20) end end
      cells[#cells+1]="";current="";column=0
    elseif text:char_at(at)==123 then local value,next_at=balanced(text,at,123,125,context);current=current.."{"..(value or "")..(next_at<text:length() and "}" or "");at=next_at;line=true
    else local point=text:char_at(at);current=current..char(point);at=at+1;if not whitespace(point) then line=true end end
  end
  if line then set_current() else cells[#cells]=nil end
  expected=expected or column
  if line and column~=expected then if expected<column then for _=1,column-expected do cells[#cells+1]=nil end else context.errors:add(20) end end
  local sep={};for i=1,math.min(#hsep,row+(line and 1 or 0)) do sep[#sep+1]=hsep[i] end
  return cells,at,expected+1,table.concat(sep)
end

local function repair_alignment(alignment,columns,context)
  local valid,count={},0
  for i=1,#alignment do local c=alignment:sub(i,i);if c~="c" and c~="l" and c~="r" and c~="|" then context.errors:add(23);c="c" end;valid[#valid+1]=c;if c~="|" then count=count+1 end end
  local source=table.concat(valid);if count==columns then return source end
  local out,aligned,index={},0,1
  while aligned<columns do local c=source:sub(index,index);out[#out+1]=c;if c~="|" then aligned=aligned+1 end;index=index%#source+1 end
  if source:sub(index,index)=="|" then out[#out+1]="|" end
  return table.concat(out)
end
local function apply_rows(hsep,alignment,context)
  local chars,row,index={},1,1
  for i=1,#hsep do chars[i]=hsep:sub(i,i);if chars[i]~="-" then chars[i]=alignment:sub(index,index);index=index%#alignment+1;row=row+1 end end
  if row-1~=#alignment then context.errors:add(24) end
  return table.concat(chars)
end
local function begin_environment(item,text,context)
  local name=item.args[1];local record=M.lookup_environment(name);local identity=record and record[2];local result=token(item.self,item.f);result.self=item.self
  if identity==data.pd_align or identity==data.pd_array then
    local row_alignment,column_alignment,begin
    if identity==data.pd_array then row_alignment,begin=M.option(text,item.next,context);column_alignment,begin=M.argument(text,begin,context);if not column_alignment or column_alignment=="" then context.errors:add(21);return result end else column_alignment="rl";begin=item.next end
    local cells,ending,columns,hsep=table_read(text,begin,context)
    if context.errors:query(20) then set_args(result,cells)
    elseif text_at(text,ending,math.min(text:length(),ending+#name+2))~="{"..name.."}" then context.errors:add(22)
    else result.p=data.pd_array;result.next=ending+Text.new(name):length()+2;set_args(result,cells);column_alignment=repair_alignment(column_alignment,columns,context);if row_alignment then hsep=apply_rows(hsep,row_alignment,context) end;set_options(result,{tostring(columns),column_alignment,hsep}) end
  elseif identity then
    local option,begin=M.option(text,item.next,context);local vertical=(option and #option>0) and option:sub(1,1) or "c";local cells,ending,columns,hsep=table_read(text,begin,context)
    if vertical~="l" and vertical~="r" and vertical~="c" then context.errors:add(23);vertical="c" end
    if text_at(text,ending,math.min(text:length(),ending+#name+2))~="{"..name.."}" then context.errors:add(22)
    else local clean={};for i=1,#hsep do local c=hsep:sub(i,i);if c=="-" then context.errors:add(25) else clean[#clean+1]=c end end;result.p=identity;result.next=ending+Text.new(name):length()+2;set_args(result,cells);set_options(result,{tostring(columns),string.rep(vertical,columns),table.concat(clean)}) end
  else context.errors:add(26) end
  if result.next and result.p~=data.pd_none then peek(result,text,result.next,context) end
  return result
end

local function sublexer(text,begin,font,context)
  local length=text:length();local item=token(begin,font)
  if begin>=length then return item end
  local point=text:char_at(begin)
  if point==92 then
    local key=M.lookup_key(text,begin);local identity=key and key[2]
    if identity==data.pd_leftright then local parts=left_middle_right(text,begin+Text.new(key[1]):length(),context);item.p=identity;set_args(item,{parts[2],parts[3],parts[4],parts[5],parts[6]});return peek(item,text,parts[1],context)
    elseif identity and identity>=data.pd_big1 and identity<=data.pd_big4 then begin=begin+Text.new(key[1]):length();while begin<length and text:char_at(begin)==32 do begin=begin+1 end;local delimiter_id,name=M.lookup_delimiter(text,begin);if delimiter_id==data.del_none then context.errors:add(16) end;item.p=identity;set_args(item,{name});return peek(item,text,begin+Text.new(name):length(),context)
    elseif identity==data.pd_function or identity==data.pd_lim then local ending=begin+1;while ending<length and not ("\\_^/*{ ,;("):find(char(text:char_at(ending)),1,true) do ending=ending+1 end;item.p=data.pd_text;item.limits=identity==data.pd_lim and 1 or 0;set_args(item,{text_at(text,begin+1,ending)});if text:char_at(ending)==32 then ending=ending+1 end;return peek(item,text,ending,context)
    elseif identity==data.pd_setitalic or identity==data.pd_setbold or identity==data.pd_setroman then begin=begin+3;item.f=identity==data.pd_setitalic and data.f_italic or identity==data.pd_setbold and data.f_bold or data.f_roman;item.p=identity==data.pd_setitalic and data.pd_mathsfit or identity==data.pd_setbold and data.pd_mathbf or data.pd_text;if text:char_at(begin)==32 then begin=begin+1 end;local remainder=text_at(text,begin);if #remainder>M.max_string_bytes then context.errors:add(27);remainder=M.truncate(remainder,context) end;set_args(item,{remainder});item.next=begin+Text.new(remainder):length();return item
    elseif identity==data.pd_endline then item.p=identity;item.next=begin+2;return item
    elseif identity==data.pd_kern then
      begin=begin+5
      local _,numeric_length=strings.number_prefix(text_at(text,begin))
      local number_end=begin+numeric_length
      local unit_end=M.command_end(text,number_end) or number_end
      local length_name=text_at(text,number_end,unit_end)
      item.p=identity
      if strings.lookup_unit(length_name)>=0 then
        set_args(item,{text_at(text,begin,unit_end)})
        item.next=math.min(length,unit_end<length and unit_end+1 or unit_end)
      elseif number_end>begin then
        set_args(item,{text_at(text,begin,number_end)})
        item.next=number_end
      else
        context.errors:add(28)
        item.p=data.pd_none
      end
      return item
    elseif key then
      item.p=identity;begin=begin+Text.new(key[1]):length();local options={};for _=1,key[4] do local value,next_at=M.option(text,begin,context);if value==nil then break end;options[#options+1]=value;begin=next_at end;local value,next_at=M.option(text,begin,context);if value~=nil then context.errors:add(29);repeat value,next_at=M.option(text,next_at,context) until value==nil;begin=next_at end;set_options(item,options)
      local args={};for _=1,key[3] do value,next_at=M.argument(text,begin,context);if value==nil then if #args>0 and context.errors:query(12) and args[#args]:sub(1,1)=="\\" then context.errors:add(30) else context.errors:add(28) end;item.p=data.pd_none;return item end;args[#args+1]=value;begin=next_at end;set_args(item,args);return peek(item,text,begin,context)
    else
      local ending=begin+1
      if ending<length and command_kind(text:char_at(ending))==0 then item.p=data.pd_symbol;set_args(item,{char(text:char_at(ending))});return peek(item,text,ending+1,context) end
      context.errors:add(30);return item
    end
  elseif point==36 then local ending=begin+1;while ending<length and text:char_at(ending)~=36 do ending=ending+1 end;item.p=data.pd_rootfont;set_args(item,{text_at(text,begin+1,ending)});if ending>=length then context.errors:add(31);return peek(item,text,length,context) end;return peek(item,text,ending+1,context)
  elseif point==123 then local value,next_at=balanced(text,begin,123,125,context);if next_at>=length then context.errors:add(32) end;item.p=data.pd_block;set_args(item,{value or ""});return peek(item,text,next_at,context)
  elseif point==39 then local ending,count=begin+1,1;while count<255 and ending<length and text:char_at(ending)==39 do count=count+1;ending=ending+1 end;if count==255 then context.errors:add(33) end;item.p=data.pd_prime;set_args(item,{tostring(count)});return peek(item,text,ending,context)
  elseif point==94 or point==95 then item.p=data.pd_box;set_args(item,{"0","1"});return peek(item,text,begin,context)
  else local ending=begin+1;while ending<length and not ("\\_^/*{ +-'"):find(char(text:char_at(ending)),1,true) do ending=ending+1 end;while ending<length and text:char_at(ending)==32 do ending=ending+1 end;local raw=text_at(text,begin,ending):gsub("[ \t]+"," ");item.p=data.pd_symbol;set_args(item,{raw});return peek(item,text,ending,context) end
end
function M.lexer(value,begin,font,context)
  local text=type(value)=="string" and Text.new(value) or value;local item=sublexer(text,begin,font,context);if item.p==data.pd_begin then return begin_environment(item,text,context) end;return item
end
function M.truncate(value,context)
  local text=Text.new(value);if text:byte_length()<=M.max_string_bytes then return text:to_string() end
  local bytes,at=0,0;while at<text:length() do local next_at=at+1;local size=text:view(at,next_at):byte_length();if bytes+size>M.max_string_bytes then break end;bytes=bytes+size;at=next_at end
  context.errors:add(27);return text_at(text,0,at)
end
local function preprocess_symbols(value)
  local text=Text.new(value);local pieces,pending,at={},{},0
  local function emit(piece) pieces[#pieces+1]=piece end
  local function flush() for i=#pending,1,-1 do emit(char(pending[i])) end;pending={} end
  while at<text:length() do local point=text:char_at(at)
    if point==92 then local key=M.lookup_key(text,at)
      if key then emit(key[1]);at=at+Text.new(key[1]):length()
      else local symbol=M.lookup_symbol(text,at)
        if symbol then local code,name=symbol[2],symbol[1];if strings.combining_mark(code) then pending[#pending+1]=code else emit(char(code));flush() end;at=at+Text.new(name):length();if text:char_at(at)==32 then at=at+1 end
        else emit("\\");at=at+1;flush() end
      end
    elseif point==10 then at=at+1 else emit(char(point));at=at+1;flush() end
  end
  return table.concat(pieces)
end
local function preprocess_greedy(value,operator)
  local text=Text.new(value);local at,op=0,Text.new(operator):length()
  while at<text:length() do
    if at+op<=text:length() and text_at(text,at,at+op)==operator then
      if M.ascii_letter(text:char_at(at+op)) then return text:to_string() end
      local raw=text:to_string();local start=at
      if at==0 or text:char_at(at-1)~=125 then local depth=1;start=math.max(0,at-1);while start>0 and depth>0 do start=start-1;local p=text:char_at(start);if p==123 then depth=depth-1 elseif p==125 then depth=depth+1 end end;raw=text_at(text,0,start).."{"..text_at(text,start,at).."}"..text_at(text,at);at=at+2;text=Text.new(raw) end
      start=at+op;if text:char_at(start)==32 then start=start+1 end
      if text:char_at(start)~=123 then local ending,depth=start,1;while ending<text:length() and depth>0 do ending=ending+1;if ending<text:length() then local p=text:char_at(ending);if p==125 then depth=depth-1 elseif p==123 then depth=depth+1 end end end;raw=text_at(text,0,start).."{"..text_at(text,start,ending).."}"..text_at(text,ending);text=Text.new(raw) end
      at=at+1
    else at=at+1 end
  end
  return text:to_string()
end
function M.preprocess(value,context) local result=preprocess_symbols(M.truncate(value,context));result=preprocess_greedy(result,"\\over");return preprocess_greedy(result,"\\choose") end
function M.tokenize(value,context,font)
  local text=Text.new(M.preprocess(value,context));local tokens,at={},0
  while at<text:length() do local item=M.lexer(text,at,font or data.f_nofont,context);tokens[#tokens+1]=item;if item.next<=at then item.next=at+1 end;at=item.next end
  return tokens
end
return M
