-- elatex -- native Lua TeX-like Unicode renderer facade
-- SPDX-License-Identifier: GPL-3.0-or-later

local data = require("elatex.data")
local Text = require("elatex.text")
local util = require("elatex.util")
local errors = require("elatex.error")
local parser = require("elatex.parser")
local box = require("elatex.box")
local draw = require("elatex.draw")

local M = {config={line_width=0,default_font="text",wide_character_width=2,full_width_character_width=2}}
local selected="unicode"
local styles={unicode=util.shallow_copy(data.style_templates.unicode),ascii=util.shallow_copy(data.style_templates.ascii)}
local published=errors.ErrorState.new()
local last_state=0

local function tagged(kind, fields)
  fields.kind=kind
  return setmetatable(fields,{__tostring=function(item) return item.kind end})
end
local function invalid_input(value) error(tagged("elatex.invalid_input",{value=value}),0) end
local function invalid_option(name,value) error(tagged("elatex.invalid_option",{name=name,value=value}),0) end
local function normalize(value)
  if type(value)~="string" then invalid_input(value) end
  local ok,text=pcall(Text.new,value)
  if not ok then invalid_input(value) end
  local nul=value:find("\0",1,true)
  return nul and value:sub(1,nul-1) or value
end
local known={line_width=true,font=true,style=true,wide_character_width=true,full_width_character_width=true,map_super_sub=true,avoid_combining=true,on_error=true,raise_on_error=true}
local function values(options)
  if options == nil then options = {} elseif type(options)~="table" then invalid_option("options",options) end
  for name in pairs(options) do if not known[name] then invalid_option(name,options[name]) end end
  local line_width=options.line_width;if line_width==nil then line_width=M.config.line_width end
  local font=options.font;if font==nil then font=M.config.default_font end
  local style=options.style;if style==nil then style=selected end
  local wide=options.wide_character_width;if wide==nil then wide=M.config.wide_character_width end
  local full=options.full_width_character_width;if full==nil then full=M.config.full_width_character_width end
  if not util.is_integer(line_width) then invalid_option("line_width",line_width) end
  if type(font)~="string" then invalid_option("font",font) end
  if style~="unicode" and style~="ascii" then invalid_option("style",style) end
  if wide~=1 and wide~=2 then invalid_option("wide_character_width",wide) end
  if full~=1 and full~=2 then invalid_option("full_width_character_width",full) end
  if options.on_error~=nil and type(options.on_error)~="function" then invalid_option("on_error",options.on_error) end
  local call_style=util.shallow_copy(styles[style])
  if options.map_super_sub~=nil then call_style.map_super_sub=options.map_super_sub and 1 or 0 end
  if options.avoid_combining~=nil then call_style.avoid_combining=options.avoid_combining and 1 or 0 end
  return {line_width=math.max(0,line_width),font=font,style=call_style,style_kind=style,wide_character_width=wide,full_width_character_width=full,errors=errors.ErrorState.new()},options
end
local function complete(context,options,output)
  local list=context.errors:list();local result={output=output,errors=list,error_state=context.errors.state}
  last_state=context.errors.state;published=errors.ErrorState.new()
  if #list>0 then
    if options.raise_on_error then error(tagged("elatex.render_error",{result=result}),0) end
    if options.on_error then
      local ok,value=pcall(options.on_error,list)
      last_state=context.errors.state;published=errors.ErrorState.new()
      if not ok then error(value,0) end
    end
  end
  return result
end
local function render_values(input, options, tree)
  input = normalize(input)
  local context, actual = values(options)
  local root = parser.parse(input, context.line_width, context.font, context)
  box.position(root,context)
  return context, actual, tree and draw.tree(root) or draw.draw(root, context)
end
local function render_impl(input, options, tree)
  local context, actual, output = render_values(input, options, tree)
  return complete(context, actual, output)
end
function M.render(input,options) return render_impl(input,options,false) end
function M.box_tree(input,options) return render_impl(input,options,true) end
function M.render_text(input)
  local context, _, output = render_values(input, nil, false)
  published = context.errors:copy()
  last_state = context.errors.state
  return output
end
function M.write(input,sink,options)
  local result=M.render(input,options)
  if type(sink)=="function" then sink(result.output) elseif type(sink)=="table" and type(sink.write)=="function" then sink:write(result.output) else error("invalid sink",0) end
  return #result.output,result
end
function M.symbols() local result={};for i=1,#data.symbols do result[i]={name=data.symbols[i][1],codepoint=data.symbols[i][2]} end;return result end
local function combining_entries()
  local result={};for i=1,#data.combining_commands do local entry=data.combining_commands[i];for j=1,#data.keys do if data.keys[j][2]==entry[1] then result[#result+1]={data.keys[j][1],entry[2]} end end end;return result
end
function M.symbols_string() local values={};for i=1,#data.symbols do values[#values+1]=data.symbols[i][1]..":"..Text.from_codepoints({data.symbols[i][2]}):to_string()..";" end;local dotted="◌";for _,entry in ipairs(combining_entries()) do values[#values+1]=entry[1].." "..dotted..":"..dotted..Text.from_codepoints({entry[2]}):to_string()..";" end;return table.concat(values) end
function M.symbols_listing() local result,max={},0;for i=1,#data.symbols do max=math.max(max,#data.symbols[i][1]) end;for i=1,#data.symbols do local item=data.symbols[i];result[#result+1]="Symbol: "..item[1]..string.rep(" ",max+2-#item[1])..Text.from_codepoints({item[2]}):to_string().."\n" end;local dotted="◌";for _,entry in ipairs(combining_entries()) do result[#result+1]="Symbol: "..entry[1].." "..dotted..string.rep(" ",max+1-#entry[1])..dotted..Text.from_codepoints({entry[2]}):to_string().."\n" end;return table.concat(result) end
function M.take_errors() local result=published:list();published=errors.ErrorState.new();return result end
function M.errors_string() return table.concat(M.take_errors(),"; ") end
function M.errors_listing() local result=M.take_errors();for i=1,#result do result[i]="ERROR: "..result[i].."\n" end;return table.concat(result) end
function M.error_state() return last_state end
function M.set_style(style) if style~="unicode" and style~="ascii" then invalid_option("style",style) end;selected=style end
function M.toggle_map_super_sub() local style=styles[selected];style.map_super_sub=style.map_super_sub==0 and 1 or 0 end
function M.toggle_avoid_combining() local style=styles[selected];style.avoid_combining=style.avoid_combining==0 and 1 or 0 end
function M.set_root_font(font) if type(font)~="string" then invalid_option("font",font) end;local record=data.key_index["\\"..font];local accepted=record and record[2] and record[2]>=data.pd_text and record[2]<=data.pd_mathnormal;M.config.default_font=accepted and font or "unknown" end
return M
