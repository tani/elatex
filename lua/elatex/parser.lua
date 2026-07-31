-- elatex.parser -- recursive TeX-like composition into retained boxes
-- SPDX-License-Identifier: GPL-3.0-or-later

local data = require("elatex.data")
local Text = require("elatex.text")
local box = require("elatex.box")
local lexer = require("elatex.lexer")
local strings = require("elatex.string")
local util = require("elatex.util")
local M = {}

local function one(point) return Text.from_codepoints({point}):to_string() end
local function line(parent, width) return box.add(parent,data.b_line,{width or 0}) end
local function unit(parent, value) return box.add(parent,data.b_unit,value) end
local function append_combining(item, mark)
  if item.type==data.b_unit then item.content=item.content..mark;return true end
  for i=#item.children,1,-1 do if append_combining(item.children[i],mark) then return true end end
  return false
end
local function count_units(item)
  if item.type==data.b_unit then return 1 end
  local total=0
  for i=1,#item.children do total=total+count_units(item.children[i]) end
  return total
end
local function split_plain(value, separator)
  local result,start={},1
  while true do
    local at=value:find(separator,start,true)
    if not at then result[#result+1]=value:sub(start);return result end
    result[#result+1]=value:sub(start,at-1)
    start=at+#separator
  end
end
local function infix_command(source, name)
  local depth,at=0,1
  while at<=#source do
    local byte=source:byte(at)
    if byte==123 and source:byte(at-1)~=92 then
      depth=depth+1
    elseif byte==125 and source:byte(at-1)~=92 then
      depth=math.max(0,depth-1)
    elseif depth==0 and source:sub(at,at+#name-1)==name then
      local next_byte=source:byte(at+#name)
      if not next_byte or not (next_byte>=65 and next_byte<=90 or next_byte>=97 and next_byte<=122) then return at end
    end
    at=at+1
  end
  return nil
end
local function matching_right(source)
  local depth,at=0,1
  while true do
    local left=source:find("\\left",at,true)
    local right=source:find("\\right",at,true)
    if not right then return nil end
    if left and left<right then depth=depth+1;at=left+5
    elseif depth==0 then return right
    else depth=depth-1;at=right+6 end
  end
end
local parse_into
local function make_fraction(parent, top, bottom, context, font)
  local frac=box.add(parent,data.b_array,{1})
  parse_into(top,line(frac),context,font)
  local bar=unit(frac,"")
  parse_into(bottom,line(frac),context,font)
  box.position(frac,context)
  bar.content=Text.from_codepoints({context.style.fracline}):to_string():rep(frac.width)
  bar.width,bar.x_center=frac.width,frac.x_center
  frac.state=data.init
  box.position(frac,context)
  box.set_state(frac,data.sizeknown)
  frac.y_center,frac.y_align=bar.ry,data.fix
end
local map_character
local function positioned_unit(parent, value, x, y)
  local child=box.add(parent,data.b_unit,value)
  parent.content[#parent.content+1],parent.content[#parent.content+2]=x,y
  return child
end
local function make_choose(parent, top, bottom, context, font)
  local result=box.add(parent,data.b_pos,{1,0})
  local stack=box.add(result,data.b_array,{1})
  parse_into(top,line(stack),context,font)
  box.add(stack,data.b_dummy,{0,1})
  parse_into(bottom,line(stack),context,font)
  box.position(stack,context)
  stack.y_center,stack.y_align,stack.state=stack.children[2].ry,data.fix,data.sizeknown
  for row=0,stack.height-1 do
    local index=row==0 and 2 or row==stack.height-1 and 4 or 3
    positioned_unit(result,one(context.style.lbrack[index]),0,row)
    positioned_unit(result,one(context.style.rbrack[index]),stack.width+1,row)
  end
  result.y_center,result.y_align=stack.y_center,data.fix
end
map_character = function(point, font)
  local upper = point >= 65 and point <= 90
  local lower = point >= 97 and point <= 122
  local digit = point >= 48 and point <= 57
  local bases = {
    [data.pd_bold]={0x1d400,0x1d41a,0x1d7ce}, [data.pd_mathbf]={0x1d400,0x1d41a,0x1d7ce},
    [data.pd_mathbfit]={0x1d468,0x1d482}, [data.pd_mathcal]={0x1d49c,0x1d4b6},
    [data.pd_mathscr]={0x1d4d0,0x1d4ea}, [data.pd_mathfrak]={0x1d504,0x1d51e},
    [data.pd_mathbb]={0x1d538,0x1d552,0x1d7d8}, [data.pd_mathsf]={0x1d5a0,0x1d5ba,0x1d7e2},
    [data.pd_mathsfbf]={0x1d5d4,0x1d5ee,0x1d7ec}, [data.pd_mathsfit]={0x1d608,0x1d622},
    [data.pd_mathsfbfit]={0x1d63c,0x1d656}, [data.pd_mathtt]={0x1d670,0x1d68a,0x1d7f6},
    [data.pd_mathnormal]={0x1d434,0x1d44e}
  }
  local greek_tables = {
    [data.pd_bold] = data.greek_bftable, [data.pd_mathbf] = data.greek_bftable,
    [data.pd_mathbfit] = data.greek_bfittable, [data.pd_mathsfbf] = data.greek_sfbftable,
    [data.pd_mathsfit] = data.greek_sfittable, [data.pd_mathsfbfit] = data.greek_sfbfittable
  }
  local greek = greek_tables[font]
  if greek then
    for i = 1, #greek do if greek[i][1] == point then return greek[i][2] end end
  end
  local base = bases[font]
  if not base then return point end
  if upper then return base[1] + point - 65 end
  if lower then return base[2] + point - 97 end
  if digit and base[3] then return base[3] + point - 48 end
  return point
end
local function make_integral(parent, count, contour, sub, super, context, font)
  local result=box.add(parent,data.b_pos,{})
  local map_script=function(value, super_p)
    if not value then return nil end
    if context.style.map_super_sub~=0 then
      if super_p and strings.mappable_super(value) then return strings.map_super(value) end
      if not super_p and strings.mappable_sub(value) then return strings.map_sub(value) end
    end
    return value
  end
  local limits=count>1
  local sub_text,super_text
  if limits then sub_text,super_text=map_script(sub,true),map_script(super,false)
  else sub_text,super_text=map_script(sub,false),map_script(super,true) end
  local base_y=limits and sub_text and 1 or (context.style.map_super_sub==0 and sub_text and 1 or 0)
  local columns=count==5 and 3 or count
  local offset=contour and 1 or 0
  for column=0,columns-1 do
    if count~=5 or column~=1 then
      for row=0,2 do
        local index=row==0 and 1 or row==2 and 3 or 2
        positioned_unit(result,one(context.style.int[index]),column+offset,base_y+row)
      end
    end
  end
  if count==5 then positioned_unit(result,one(context.style.iint[4]),offset+1,base_y+1) end
  local width=columns+offset*2
  local script_box=function(value)
    if not value then return nil end
    local child=line(result)
    parse_into(value,child,context,font)
    box.position(child,context)
    return child
  end
  if contour then
    positioned_unit(result,one(context.style.oint[1]),0,base_y+1)
    positioned_unit(result,one(context.style.oint[2]),columns+1,base_y+1)
  end
  if limits then
    local sub_box,super_box=script_box(sub_text),script_box(super_text)
    if sub_box then result.content[#result.content+1],result.content[#result.content+2]=math.max(0,util.cdiv(width-sub_box.width,2)),0 end
    if super_box then result.content[#result.content+1],result.content[#result.content+2]=math.max(0,util.cdiv(width-super_box.width,2)),base_y+3 end
  else
    local sub_box,super_box=script_box(sub_text),script_box(super_text)
    if sub_box then result.content[#result.content+1],result.content[#result.content+2]=width,context.style.map_super_sub==0 and 0 or base_y end
    if super_box then result.content[#result.content+1],result.content[#result.content+2]=width,base_y+2+(context.style.map_super_sub==0 and 1 or 0) end
  end
  result.y_center,result.y_align=base_y+1,data.fix
end
local function make_large(parent, characters, sub, super, context, font)
  local result=box.add(parent,data.b_pos,{})
  local width,height=characters[1],characters[2]
  local sub_text,super_text=sub,super
  if context.style.map_super_sub~=0 then
    if sub and strings.mappable_super(sub) then sub_text=strings.map_super(sub) end
    if super and strings.mappable_sub(super) then super_text=strings.map_sub(super) end
  end
  local base_y=sub_text and 1 or 0
  for row=0,height-1 do
    for column=0,width-1 do
      positioned_unit(result,one(characters[3+row*width+column]),column,base_y+row)
    end
  end
  local script_box=function(value)
    if not value then return nil end
    local child=line(result)
    parse_into(value,child,context,font)
    box.position(child,context)
    return child
  end
  local sub_box,super_box=script_box(sub_text),script_box(super_text)
  if sub_box then result.content[#result.content+1],result.content[#result.content+2]=math.max(0,util.cdiv(width-sub_box.width,2)),0 end
  if super_box then result.content[#result.content+1],result.content[#result.content+2]=math.max(0,util.cdiv(width-super_box.width,2)),base_y+height end
  result.y_center,result.y_align=base_y+util.cdiv(height,2),data.fix
end
local function make_array(parent, source, columns, context, font)
  local rows=split_plain(source,"\\\\")
  local result=box.add(parent,data.b_array,{columns})
  for row=1,#rows do
    local cells=split_plain(rows[row],"&")
    for column=1,columns do parse_into(cells[column] or "",line(result),context,font) end
  end
  return result
end
local function make_wrapped_array(parent, source, columns, left, right, context, font)
  local result=box.add(parent,data.b_pos,{1,0})
  local array=make_array(result,source,columns,context,font)
  box.position(array,context)
  for row=0,array.height-1 do
    local index
    if left==context.style.lcurly then index=row==0 and 2 or row==array.height-1 and 5 or 3
    elseif left==context.style.vbar or left==context.style.dbar then index=1
    else index=row==0 and 2 or row==array.height-1 and 4 or 3 end
    positioned_unit(result,one(left[index]),0,row)
    positioned_unit(result,one(right[index]),array.width+1,row)
  end
  result.y_center,result.y_align=array.y_center,data.fix
end
local function make_cases(parent, source, columns, context, font)
  local result=box.add(parent,data.b_pos,{1,0})
  local array=make_array(result,source,columns,context,font)
  box.position(array,context)
  for row=0,array.height-1 do
    local index=row==0 and 2 or row==array.height-1 and 5 or 3
    positioned_unit(result,one(context.style.lcurly[index]),0,row)
  end
  result.y_center,result.y_align=array.y_center,data.fix
end
local function make_stack(parent, top, bottom, distance, context, font)
  local result=box.add(parent,data.b_array,{1})
  parse_into(top,line(result),context,font)
  if distance>0 then box.add(result,data.b_dummy,{0,distance}) end
  parse_into(bottom,line(result),context,font)
  box.position(result,context)
  result.y_center,result.y_align=result.children[2].ry,data.fix
  return result
end
local function make_binom(parent, top, bottom, context, font)
  local result=box.add(parent,data.b_pos,{1,0})
  local stack=make_stack(result,top,bottom,1,context,font)
  for row=0,stack.height-1 do
    local index=row==0 and 2 or row==stack.height-1 and 4 or 3
    positioned_unit(result,one(context.style.lbrack[index]),0,row)
    positioned_unit(result,one(context.style.rbrack[index]),stack.width+1,row)
  end
  result.y_center,result.y_align=stack.y_center,data.fix
end
local function make_limit(parent, sub, super, context, font)
  local result=box.add(parent,data.b_pos,{})
  local base_y=sub and 1 or 0
  positioned_unit(result,"lim",0,base_y)
  if sub then
    local script=line(result);parse_into(sub,script,context,font);box.position(script,context)
    result.content[#result.content+1],result.content[#result.content+2]=math.max(0,util.cdiv(3-script.width,2)),0
  end
  if super then
    local script=line(result);parse_into(super,script,context,font);box.position(script,context)
    result.content[#result.content+1],result.content[#result.content+2]=math.max(0,util.cdiv(3-script.width,2)),base_y+1
  end
  result.y_center,result.y_align=base_y,data.fix
end
local function make_big_delimiter(parent, delimiter, height, context)
  local styles={
    ["("]=context.style.lbrack,[")"]=context.style.rbrack,["["]=context.style.lsquare,["]"]=context.style.rsquare,
    ["{"]=context.style.lcurly,["}"]=context.style.rcurly,["\\{"]=context.style.lcurly,["\\}"]=context.style.rcurly,
    ["|"]=context.style.vbar,["‖"]=context.style.dbar,
  }
  local characters=styles[delimiter]
  if not characters then return false end
  if (delimiter=="{" or delimiter=="}" or delimiter=="\\{" or delimiter=="\\}") and height%2==0 then height=height+1 end
  local result=box.add(parent,data.b_pos,{})
  if height==1 then positioned_unit(result,one(characters[1]),0,0)
  elseif characters==context.style.lcurly or characters==context.style.rcurly then
    for row=0,height-1 do
      local index=row==0 and 2 or row==util.cdiv(height,2) and 3 or row==height-1 and 5 or 4
      positioned_unit(result,one(characters[index]),0,row)
    end
  else
    positioned_unit(result,one(characters[2]),0,0)
    for row=1,height-2 do positioned_unit(result,one(characters[3]),0,row) end
    positioned_unit(result,one(characters[4]),0,height-1)
  end
  result.y_center,result.y_align=util.cdiv(height-1,2),data.fix
  return true
end
local function make_marked(parent, source, identity, above, repeat_mark, right_align, context, font)
  local record=data.combining_command_index[identity]
  local point=record[3]~=0 and record[3] or record[2]
  if context.style_kind=="ascii" and record[4]~=0 then point=record[4] end
  local result=box.add(parent,data.b_array,{1})
  local add_mark=function(width)
    local mark=unit(result,one(point):rep(repeat_mark and width or 1))
    if right_align then mark.x_align=data.max end
  end
  if above then
    local probe=box.new(nil,data.b_line,{0});parse_into(source,probe,context,font);box.size(probe,context);box.position(probe,context)
    add_mark(probe.width)
    local body=line(result);parse_into(source,body,context,font)
    if right_align then body.x_align=data.max end
  else
    local body=line(result);parse_into(source,body,context,font)
    if right_align then body.x_align=data.max end
    box.position(body,context)
    add_mark(body.width)
  end
  box.position(result,context)
  local body=above and result.children[2] or result.children[1]
  result.y_center,result.y_align=body.ry+body.y_center,data.fix
  return result
end
parse_into = function(source,parent,context,font)
  local over=infix_command(source,"\\over")
  local choose=infix_command(source,"\\choose")
  if over then
    local top=source:sub(1,over-1):gsub("^[ \t\n\r]+",""):gsub("[ \t\n\r]+$","")
    local bottom=source:sub(over+5):gsub("^[ \t\n\r]+",""):gsub("[ \t\n\r]+$","")
    make_fraction(parent,top,bottom,context,font)
    return
  elseif choose then
    local top=source:sub(1,choose-1):gsub("^[ \t\n\r]+",""):gsub("[ \t\n\r]+$","")
    local bottom=source:sub(choose+7):gsub("^[ \t\n\r]+",""):gsub("[ \t\n\r]+$","")
    make_choose(parent,top,bottom,context,font)
    return
  end
  local text,at,force_limits=Text.new(source),0,false
  while at<text:length() do
    local target=line(parent)
    local point=text:char_at(at)
    if point==123 then
      local argument,next_at=lexer.argument(text,at,context)
      local over=argument and argument:find("\\over",1,true)
      local choose=argument and argument:find("\\choose",1,true)
      if argument then
        if over then
          local top=argument:sub(1,over-1):gsub("^[ \t\n\r]+",""):gsub("[ \t\n\r]+$","")
          local bottom=argument:sub(over+5):gsub("^[ \t\n\r]+",""):gsub("[ \t\n\r]+$","")
          make_fraction(target,top,bottom,context,font)
        elseif choose then
          make_choose(target,argument:sub(1,choose-1):gsub("^[ \t\n\r]+",""):gsub("[ \t\n\r]+$",""),argument:sub(choose+7):gsub("^[ \t\n\r]+",""):gsub("[ \t\n\r]+$",""),context,font)
        else parse_into(argument,target,context,font) end
        at=next_at
      else at=at+1 end
    elseif point==125 then context.errors:add(32);at=at+1
    elseif point==92 then
      local command,next_at=lexer.command(text,at)
      if command=="\\frac" or command=="\\dfrac" or command=="\\tfrac" then
        local top,after_top=lexer.argument(text,next_at,context);local bottom,after_bottom=lexer.argument(text,after_top,context)
        if not top or not bottom then context.errors:add(28);at=after_bottom else make_fraction(target,top,bottom,context,font);at=after_bottom end
      elseif command=="\\{" or command=="\\}" or command=="\\_" or command=="\\%" or command=="\\#" or command=="\\&" or command=="\\$" then
        unit(target,command:sub(2));at=next_at
      elseif command=="\\backslash" then
        unit(target,"\\");at=next_at
      elseif command=="\\mathop" or command=="\\mathord" then
        local body,after_body=lexer.argument(text,next_at,context)
        if not body then context.errors:add(28);at=next_at else parse_into(body,target,context,font);at=after_body end
      elseif command=="\\displaystyle" then
        table.remove(parent.children);at=next_at
      elseif command=="\\limits" or command=="\\nolimits" then
        table.remove(parent.children)
        if command=="\\limits" and #parent.children>0 then
          local holder=box.new(parent,data.b_line,{0})
          holder.children=parent.children
          for i=1,#holder.children do holder.children[i].parent=holder end
          parent.children={holder}
          force_limits=true
        else force_limits=false end
        at=next_at
      elseif command=="\\stackrel" or command=="\\stackbin" or command=="\\binom" then
        local top,after_top=lexer.argument(text,next_at,context)
        local bottom,after_bottom=lexer.argument(text,after_top,context)
        if not top or not bottom then context.errors:add(28);at=after_bottom
        elseif command=="\\binom" then make_binom(target,top,bottom,context,font);at=after_bottom
        else make_stack(target,top,bottom,0,context,font);at=after_bottom end
      elseif command=="\\raisebox" then
        local height, after_height=lexer.argument(text,next_at,context)
        local body, after_body=lexer.argument(text,after_height,context)
        if not height or not body then context.errors:add(28);at=after_body else
          parse_into(body,target,context,font)
          box.position(target,context)
          target.y_center=target.y_center-strings.read_length_height(height)
          target.y_align=data.fix
          target.state=data.sizeknown
          at=after_body
        end
      elseif command=="\\box" or command=="\\kern" then
        local width, after_width=lexer.argument(text,next_at,context)
        local height, after_height
        if command=="\\box" then height,after_height=lexer.argument(text,after_width,context) else height,after_height="2",after_width end
        if not width or not height then context.errors:add(28);at=after_height else
          box.add(target,data.b_dummy,{strings.read_length_width(width),strings.read_length_height(height)})
          at=after_height
        end
      elseif command=="\\phantom" or command=="\\hphantom" or command=="\\vphantom" then
        local body,next_body=lexer.argument(text,next_at,context)
        if not body then context.errors:add(28);at=next_at else
          local measured=box.new(nil,data.b_line,{0})
          parse_into(body,measured,context,font)
          box.position(measured,context)
          local horizontal=command~="\\vphantom"
          local vertical=command~="\\hphantom"
          box.add(target,data.b_dummy,{horizontal and measured.width or 0,vertical and measured.height or 0})
          at=next_body
        end
      elseif command=="\\sqrt" then
        local index,after_index=lexer.option(text,next_at,context)
        local body,next_body=lexer.argument(text,after_index,context)
        if not body then context.errors:add(28);at=after_index else
          local sqrt=box.add(target,data.b_pos,{})
          local offset,x_offset=0,0
          if index then
            local index_box=line(sqrt)
            parse_into(index,index_box,context,font)
            box.position(index_box,context)
            x_offset=index_box.width-1
            sqrt.content[#sqrt.content+1],sqrt.content[#sqrt.content+2]=0,1
            offset=1
          end
          local body_box=line(sqrt)
          parse_into(body,body_box,context,font)
          box.position(body_box,context)
          local width,height=body_box.width,body_box.height
          if index then sqrt.content[2]=util.cdiv(height,2)+1 end
          sqrt.content[#sqrt.content+1],sqrt.content[#sqrt.content+2]=util.cdiv(height,2)+2+x_offset,0
          local characters=context.style.sqrt
          for row=0,height-1 do positioned_unit(sqrt,one(characters[2]),util.cdiv(height,2)+x_offset+1,row) end
          for index_at=0,util.cdiv(height,2) do positioned_unit(sqrt,one(characters[1]),index_at+x_offset,util.cdiv(height,2)-index_at) end
          positioned_unit(sqrt,one(characters[3]),util.cdiv(height,2)+1+x_offset,height)
          for index_at=0,width-1 do positioned_unit(sqrt,one(characters[4]),util.cdiv(height,2)+2+x_offset+index_at,height) end
          positioned_unit(sqrt,one(characters[5]),util.cdiv(height,2)+2+x_offset+width,height)
          sqrt.y_center,sqrt.y_align=body_box.y_center,data.fix
          at=next_body
        end
      elseif command=="\\int" or command=="\\iint" or command=="\\iiint" or command=="\\iiiint" or command=="\\idotsint" or command=="\\oint" or command=="\\oiint" or command=="\\oiiint" or command=="\\oiiiint" or command=="\\oidotsint" then
        local counts={["\\int"]=1,["\\iint"]=2,["\\iiint"]=3,["\\iiiint"]=4,["\\idotsint"]=5,["\\oint"]=1,["\\oiint"]=2,["\\oiiint"]=3,["\\oiiiint"]=4,["\\oidotsint"]=5}
        local count=counts[command]
        local contour=command:sub(2,2)=="o"
        local sub,super,after=nil,nil,next_at
        while text:char_at(after)==94 or text:char_at(after)==95 do
          local marker=text:char_at(after)
          local value,next_script=lexer.script(text,after+1,context)
          if marker==94 then super=value else sub=value end
          after=next_script
        end
        make_integral(target,count,contour,sub,super,context,font)
        at=after
      elseif command=="\\sum" or command=="\\prod" then
        local sub,super,after=nil,nil,next_at
        while text:char_at(after)==94 or text:char_at(after)==95 do
          local marker=text:char_at(after)
          local value,next_script=lexer.script(text,after+1,context)
          if marker==94 then super=value else sub=value end
          after=next_script
        end
        make_large(target,command=="\\sum" and context.style.sum or context.style.prod,sub,super,context,font)
        at=after
      elseif command=="\\lim" then
        local sub,super,after=nil,nil,next_at
        while text:char_at(after)==94 or text:char_at(after)==95 do
          local marker=text:char_at(after)
          local value,next_script=lexer.script(text,after+1,context)
          if marker==94 then super=value else sub=value end
          after=next_script
        end
        make_limit(target,sub,super,context,font)
        at=after
      elseif command=="\\pmod" or command=="\\mod" or command=="\\bmod" or command=="\\pod" then
        local body,next_body=lexer.argument(text,next_at,context)
        if not body then context.errors:add(28);at=next_at else
          if command=="\\pmod" then unit(target,"(mod ") elseif command=="\\pod" then unit(target,"(") else unit(target,"mod ") end
          parse_into(body,target,context,font)
          if command=="\\pmod" or command=="\\pod" then unit(target,")") else unit(target," ") end
          at=next_body
        end
      elseif command=="\\ " or command=="\\space" or command=="\\," then unit(target," ");at=next_at
      elseif command=="\\:" then unit(target," ");at=next_at
      elseif command=="\\quad" then unit(target,"   ");at=next_at
      elseif command=="\\qquad" then unit(target,"    ");at=next_at
      elseif command=="\\-" then unit(target,"-");at=next_at
      elseif command=="\\big" or command=="\\Big" or command=="\\bigg" or command=="\\Bigg"
        or command=="\\bigl" or command=="\\Bigl" or command=="\\biggl" or command=="\\Biggl"
        or command=="\\bigr" or command=="\\Bigr" or command=="\\biggr" or command=="\\Biggr" then
        local delimiter,after_delimiter
        if text:char_at(next_at)==92 then delimiter,after_delimiter=lexer.command(text,next_at)
        else delimiter,after_delimiter=one(text:char_at(next_at)),next_at+1 end
        local height=command:find("Bigg",1,true) and 5 or command:find("bigg",1,true) and 4 or command:find("Big",1,true) and 3 or 2
        if not delimiter or not make_big_delimiter(target,delimiter,height,context) then context.errors:add(30);unit(target,command) end
        at=after_delimiter or next_at
      elseif command=="\\;" then unit(target,"   ");at=next_at
      elseif command=="\\\\" then target.type,target.state=data.b_endline,data.sizeknown;at=next_at
      elseif command=="\\it" then font=data.pd_mathsfit;at=next_at;if text:char_at(at)==32 then at=at+1 end
      elseif command=="\\rm" then font=data.pd_text;at=next_at;if text:char_at(at)==32 then at=at+1 end
      elseif command=="\\bf" then font=data.pd_mathbf;at=next_at;if text:char_at(at)==32 then at=at+1 end
      else
        local symbol=data.symbol_index[command]
        if symbol then unit(target,one(map_character(symbol[2],font)));at=next_at
        else
          local key=data.key_index[command]
          if key and key[2] >= data.pd_comb_grave and key[2] <= data.pd_comb_vertoverlay then
            local body,next_body=lexer.argument(text,next_at,context)
            if not body then context.errors:add(28);at=next_at else
              local measure=box.new(nil,data.b_line,{0})
              parse_into(body,measure,context,font);box.size(measure,context)
              local combining=data.combining_command_index[key[2]]
              local alternative=combining[3]
              if context.style_kind=="ascii" and combining[4]~=0 then alternative=combining[4] end
              if context.style.avoid_combining==0 and not (context.style_kind=="ascii" and combining[4]~=0) and count_units(measure)==1 then
                parse_into(body,target,context,font)
                append_combining(target,one(combining[2]))
              elseif alternative~=0 then
                local below=key[2]==data.pd_comb_underline or key[2]==data.pd_comb_utilde or key[2]==data.pd_comb_wideutilde or key[2]>=data.pd_comb_threeunderdot
                make_marked(target,body,key[2],not below,key[2]==data.pd_comb_overline or key[2]==data.pd_comb_underline,key[2]==data.pd_comb_ocommatopright or key[2]==data.pd_comb_droang,context,font)
              else
                parse_into(body,target,context,font)
              end
              at=next_body
            end
          elseif key and key[2]==data.pd_function then unit(target,command:sub(2));at=next_at
          elseif key and (key[2]==data.pd_bold or key[2]==data.pd_text or key[2]>=data.pd_mathbf and key[2]<=data.pd_mathnormal) then local body,next_body=lexer.argument(text,next_at,context);if body then parse_into(body,target,context,key[2]);at=next_body else context.errors:add(28);at=next_at end
          elseif command=="\\begin" then
            local environment,after_environment=lexer.argument(text,next_at,context)
            local supported=environment=="array" or environment=="aligned" or environment=="align" or environment=="align*"
              or environment=="matrix" or environment=="matrix*" or environment=="pmatrix" or environment=="pmatrix*"
              or environment=="bmatrix" or environment=="bmatrix*" or environment=="Bmatrix" or environment=="Bmatrix*"
              or environment=="vmatrix" or environment=="vmatrix*" or environment=="Vmatrix" or environment=="Vmatrix*"
              or environment=="cases" or environment=="cases*" or environment=="dcases"
            if not supported then context.errors:add(30);unit(target,command);at=next_at
            else
              local after_content,columns=after_environment,nil
              if environment=="array" then
                local alignment,after_option
                _,after_option=lexer.option(text,after_environment,context)
                alignment,after_content=lexer.argument(text,after_option,context)
                columns=alignment and Text.new(alignment):length()
              end
              local rest=text:view(after_content):to_string()
              local ending="\\end{"..environment.."}"
              local ending_at=rest:find(ending,1,true)
              if not columns and ending_at then columns=#split_plain(split_plain(rest:sub(1,ending_at-1),"\\\\")[1],"&") end
              if not columns or not ending_at then context.errors:add(28);at=text:length()
              else
                local wrappers={
                  ["pmatrix"]={context.style.lbrack,context.style.rbrack}, ["pmatrix*"]={context.style.lbrack,context.style.rbrack},
                  ["bmatrix"]={context.style.lsquare,context.style.rsquare}, ["bmatrix*"]={context.style.lsquare,context.style.rsquare},
                  ["Bmatrix"]={context.style.lcurly,context.style.rcurly}, ["Bmatrix*"]={context.style.lcurly,context.style.rcurly},
                  ["vmatrix"]={context.style.vbar,context.style.vbar}, ["vmatrix*"]={context.style.vbar,context.style.vbar},
                  ["Vmatrix"]={context.style.dbar,context.style.dbar}, ["Vmatrix*"]={context.style.dbar,context.style.dbar},
                }
                local wrapper=wrappers[environment]
                if environment=="cases" or environment=="cases*" or environment=="dcases" then make_cases(target,rest:sub(1,ending_at-1),columns,context,font)
                elseif wrapper then make_wrapped_array(target,rest:sub(1,ending_at-1),columns,wrapper[1],wrapper[2],context,font)
                else make_array(target,rest:sub(1,ending_at-1),columns,context,font) end
                at=after_content+Text.new(rest:sub(1,ending_at-1+#ending)):length()
              end
            end
          elseif command=="\\left" then
            local rest=text:view(next_at):to_string()
            local right_at=matching_right(rest)
            local opening=rest:sub(1,1)
            local opening_length=1
            if opening=="\\" then opening=rest:match("^\\%a+") or rest:sub(1,2);opening_length=#opening end
            if not right_at or #rest<right_at+6 then context.errors:add(17);at=text:length() else
              local close_rest=rest:sub(right_at+6)
              local closing=close_rest:sub(1,1)
              if closing=="\\" then closing=close_rest:match("^\\%a+") or close_rest:sub(1,2)
              else closing=one(Text.new(close_rest):char_at(0)) end
              local body=rest:sub(opening_length+1,right_at-1)
              local result=box.add(target,data.b_pos,{})
              local body_box=line(result)
              parse_into(body,body_box,context,font)
              box.position(body_box,context)
              local height=body_box.height
              result.content[#result.content+1],result.content[#result.content+2]=1,0
              if opening=="(" and closing==")" or opening=="[" and closing=="]" then
                local left,right=opening=="(" and context.style.lbrack or context.style.lsquare,opening=="(" and context.style.rbrack or context.style.rsquare
                for row=0,height-1 do
                  local index=height==1 and 1 or row==0 and 2 or row==height-1 and 4 or 3
                  positioned_unit(result,one(left[index]),0,row)
                  positioned_unit(result,one(right[index]),body_box.width+1,row)
                end
              elseif opening=="{" and closing=="}" then
                local left,right=context.style.lcurly,context.style.rcurly
                for row=0,height-1 do
                  local index=height==1 and 1 or row==0 and 2 or row==height-1 and 5 or 3
                  positioned_unit(result,one(left[index]),0,row)
                  positioned_unit(result,one(right[index]),body_box.width+1,row)
                end
              elseif (opening=="\\lceil" and closing=="\\rceil") or (opening=="\\lfloor" and closing=="\\rfloor")
                or (opening=="\\uparrow" and closing=="\\uparrow") or (opening=="\\downarrow" and closing=="\\downarrow") then
                local pairs={
                  ["\\lceil"]={context.style.lceil,context.style.rceil},
                  ["\\lfloor"]={context.style.lfloor,context.style.rfloor},
                  ["\\uparrow"]={context.style.uparrow,context.style.uparrow},
                  ["\\downarrow"]={context.style.downarrow,context.style.downarrow},
                }
                local left,right=pairs[opening][1],pairs[opening][2]
                for row=0,height-1 do
                  local index=height==1 and 1 or row==0 and 2 or row==height-1 and 4 or 3
                  positioned_unit(result,one(left[index]),0,row)
                  positioned_unit(result,one(right[index]),body_box.width+1,row)
                end
              elseif opening=="<" and closing==">" then
                local draw_height=height
                if draw_height~=1 and draw_height%2==1 then draw_height=draw_height+1 end
                local half=util.cdiv(draw_height,2)
                result.content[1]=2
                for row=0,draw_height-1 do
                  local upper=row<half
                  local left_index=upper and 3 or 4
                  local left_x=upper and half-row-1 or row-half
                  local right_index=upper and 4 or 3
                  local right_x=upper and row or draw_height-row-1
                  positioned_unit(result,one(context.style.angle[left_index]),left_x,row)
                  positioned_unit(result,one(context.style.angle[right_index]),4+right_x,row)
                end
              elseif opening=="|" and (closing=="\\rangle" or closing=="⟩") then
                local left_body,right_body=body:match("^(.-)\\middle\\mid(.*)$")
                if not left_body then left_body,right_body=body:match("^(.-)\\middle|(.*)$") end
                if left_body then left_body=left_body:gsub("[ \t\n\r]+$","");right_body=right_body:gsub("^[ \t\n\r]+","") end
                if left_body then
                  result.children,result.content={},{}
                  local left_box=line(result);parse_into(left_body,left_box,context,font);box.position(left_box,context)
                  result.content[#result.content+1],result.content[#result.content+2]=1,0
                  positioned_unit(result,one(context.style.vbar[1]),left_box.width+1,0)
                  local right_box=line(result);parse_into(right_body,right_box,context,font);box.position(right_box,context)
                  result.content[#result.content+1],result.content[#result.content+2]=left_box.width+2,0
                  positioned_unit(result,one(context.style.vbar[1]),0,0)
                  positioned_unit(result,one(context.style.angle[2]),left_box.width+right_box.width+2,0)
                else
                  positioned_unit(result,one(context.style.vbar[1]),0,0)
                  positioned_unit(result,one(context.style.angle[2]),body_box.width+1,0)
                end
              elseif opening=="|" and closing=="|" then
                for row=0,height-1 do
                  positioned_unit(result,one(context.style.vbar[1]),0,row)
                  positioned_unit(result,one(context.style.vbar[1]),body_box.width+1,row)
                end
              elseif closing=="." and (opening=="(" or opening=="[" or opening=="{" or opening=="|") then
                local left=opening=="(" and context.style.lbrack or opening=="[" and context.style.lsquare or opening=="{" and context.style.lcurly or context.style.vbar
                for row=0,height-1 do
                  local index=opening=="|" and 1 or opening=="{" and (row==0 and 2 or row==height-1 and 5 or 3) or (row==0 and 2 or row==height-1 and 4 or 3)
                  positioned_unit(result,one(left[index]),0,row)
                end
              elseif opening=="." and (closing==")" or closing=="]" or closing=="}") then
                result.content[1]=0
                for row=0,height-1 do
                  local index=closing=="}" and (row==0 and 2 or row==height-1 and 5 or 3) or (row==0 and 2 or row==height-1 and 4 or 3)
                  positioned_unit(result,one(right[index]),body_box.width,row)
                end
              else
                positioned_unit(result,opening,0,0)
                positioned_unit(result,closing,body_box.width+1,0)
              end
              result.y_center,result.y_align=body_box.y_center,data.fix
              at=next_at+Text.new(rest:sub(1,right_at+5+#closing)):length()
            end
          elseif command=="\\right" then at=next_at
          else context.errors:add(30);unit(target,command);at=next_at end
        end
      end
    elseif point==39 then
      local count=0
      while text:char_at(at+count)==39 do count=count+1 end
      if context.style_kind=="ascii" then unit(target,string.rep("'",count))
      else
        local table_name=count==1 and "prime" or count==2 and "dprime" or count==3 and "tprime" or count==4 and "qprime"
        if table_name then unit(target,one(context.style[table_name][3])) else unit(target,string.rep(one(context.style.prime[3]),count)) end
      end
      at=at+count
    elseif point==94 or point==95 then
      local script,next_at=lexer.script(text,at+1,context)
      local base=parent.children[#parent.children-1]
      if not script then context.errors:add(12);at=at+1
      elseif base and not force_limits and point==94 and context.style.map_super_sub~=0 and strings.mappable_super(script) then
        table.remove(parent.children)
        append_combining(base,strings.map_super(script))
        at=next_at
      elseif base and not force_limits and point==95 and context.style.map_super_sub~=0 and strings.mappable_sub(script) then
        table.remove(parent.children)
        append_combining(base,strings.map_sub(script))
        at=next_at
      elseif base then
        if force_limits and context.style.map_super_sub~=0 then
          if point==94 and strings.mappable_sub(script) then script=strings.map_sub(script)
          elseif point==95 and strings.mappable_super(script) then script=strings.map_super(script) end
        end
        table.remove(parent.children)
        box.position(base,context)
        local width,height,y_center=base.width,base.height,base.y_center
        box.wrap(base,data.b_pos,{0,0,width,point==94 and height or 0},context)
        base.y_center,base.y_align=y_center,data.fix
        local script_box=line(base)
        parse_into(script,script_box,context,font)
        box.position(script_box,context)
        if force_limits then base.content[3]=math.max(0,util.cdiv(width-script_box.width,2)) end
        if point==95 then
          base.content[2]=script_box.height
          base.y_center=script_box.height
        end
        base.state=data.init
        box.position(base,context)
        box.set_state(base,data.sizeknown)
        at=next_at
      else
        parse_into(script,target,context,font)
        at=next_at
      end
    elseif point==36 or point==10 or point==13 then at=at+1
    else unit(target,one(map_character(point,font)));at=at+1 end
  end
end
function M.parse(source,line_width,font_name,context)
  local root=box.new(nil,data.b_line,{line_width})
  context.root_font=lexer.lookup_font(font_name,context)
  parse_into(lexer.truncate(source,context),root,context,context.root_font)
  return root
end
return M
