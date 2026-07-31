local harness=require("test.harness")
local fixtures=require("test.fixtures")
local elatex=require("elatex")
local util=require("elatex.util")
local oracle=os.getenv("ELATEX_ORACLE")
if not oracle or oracle=="" then return false end

local function quote(value) return "'"..value:gsub("'", "'\\''").."'" end
local function option_argv(option)
  if option=="" then return {} end
  if option=="-S" or option=="-A" or option=="-m" or option=="-a" then return {option} end
  local name=option:match("^-F (.+)$")
  if name then return {"-F",name} end
  error("unsupported oracle option: "..tostring(option),0)
end
local function read_file(path) local handle=assert(io.open(path,"rb"));local value=handle:read("*a");handle:close();return value end
local function run_oracle(entry)
  local input,output,stderr=os.tmpname(),os.tmpname(),os.tmpname()
  local function cleanup() os.remove(input);os.remove(output);os.remove(stderr) end
  local ok,value=pcall(function()
    local handle=assert(io.open(input,"wb"));handle:write(entry.input);handle:close()
    local argv=option_argv(entry.option);local args={quote(oracle)}
    for i=1,#argv do args[#args+1]=quote(argv[i]) end
    local command="LC_ALL=C.UTF-8 LANG=C.UTF-8 "..table.concat(args," ").." < "..quote(input).." > "..quote(output).." 2> "..quote(stderr)
    local a,b,c=os.execute(command);local succeeded,status=util.normalize_execute_result(a,b,c)
    local out,err=read_file(output),read_file(stderr)
    if not succeeded then error("oracle failed (status "..status.."): "..err,0) end
    assert(out:sub(-1)=="\n","oracle output lacks final LF")
    return out:sub(1,-2)
  end)
  cleanup()
  if not ok then error(value,0) end
  return value
end
for _,entry in ipairs(fixtures.all()) do
  harness.test("oracle/"..entry.file.."/b"..entry.block_index.."/r"..entry.reference_index.."/a"..entry.alternative_index,function()
    local result=run_oracle(entry)
    harness.equal(result,entry.expected,"oracle disagrees with fixture")
    harness.equal(elatex.render(entry.input,fixtures.options(entry)).output,entry.expected,"Lua disagrees with fixture")
  end)
end
return true
