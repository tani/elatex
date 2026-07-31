local M={specifications={{"testsuite.txt",94,161,377},{"testeqs.txt",29,61,94},{"testfonts.txt",6,20,33}}}
local function trim(value) return (value:gsub("^[ \t\n\r]+",""):gsub("[ \t\n\r]+$", "")) end
local function header_options(line)
  local rest=line:sub(line:find("<ref>",1,true)+5);local options={}
  for value in (rest.."|"):gmatch("(.-)|") do value=trim(value);assert(value=="" or value=="-S" or value=="-A" or value=="-m" or value=="-a" or value:match("^-F [^ \t\n\r]+$"),"unknown fixture option: "..value);options[#options+1]=value end
  return options
end
function M.load(name)
  local handle=assert(io.open("test/fixtures/"..name,"rb"));local source=handle:read("*a");handle:close();local state,blocks,references,block_index,reference_index="outside",0,0,nil,nil;local entries,input,ref_lines,options={},nil,nil,nil;local function finish()
    local expected=table.concat(ref_lines);assert(expected:sub(-1)=="\n","fixture reference lacks final LF");expected=expected:sub(1,-2)
    for alternative,option in ipairs(options) do entries[#entries+1]={file=name,block_index=block_index,reference_index=reference_index,alternative_index=alternative-1,input=input,expected=expected,option=option} end
    references=references+1;ref_lines,options=nil,nil
  end
  for with_lf in source:gmatch("[^\n]*\n") do
    local line=with_lf:sub(1,-2);local field=line:match("^[ \t]*([^ \t]+)")
    if field=="<input>" then assert(state=="outside","nested fixture input");state="input";block_index=blocks;reference_index=nil;input=""
    elseif field=="<ref>" then assert(state=="input" or state=="reference","reference outside block");if state=="reference" then finish() else input=input end;state="reference";reference_index=(reference_index or -1)+1;options=header_options(line);ref_lines={}
    elseif field=="<end>" then assert(state=="reference","end outside reference");finish();blocks=blocks+1;state="outside"
    elseif state=="input" then input=input..with_lf elseif state=="reference" then ref_lines[#ref_lines+1]=with_lf end
  end
  assert(state=="outside","unterminated fixture");return entries,blocks,references
end
function M.all()
  local result={};for _,spec in ipairs(M.specifications) do local entries,blocks,references=M.load(spec[1]);assert(blocks==spec[2] and references==spec[3] and #entries==spec[4],"fixture structure changed: "..spec[1]);for _,entry in ipairs(entries) do result[#result+1]=entry end end;return result
end
function M.options(entry)
  if entry.option=="" or entry.option=="-S" then return {style="unicode",font="text"} end
  if entry.option=="-A" then return {style="ascii",font="text",map_super_sub=false,avoid_combining=false} end
  if entry.option=="-m" then return {style="unicode",font="text",map_super_sub=false} end
  if entry.option=="-a" then return {style="unicode",font="text",avoid_combining=true} end
  return {style="unicode",font=entry.option:match("^-F (.+)$")}
end
return M
