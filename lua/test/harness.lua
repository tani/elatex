local M={tests={},failures={}}
function M.test(name,fn) M.tests[#M.tests+1]={name,fn} end
function M.equal(actual,expected,message) if actual~=expected then error((message or "values differ").."\nexpected: "..string.format("%q",expected).."\nactual: "..string.format("%q",actual),2) end end
function M.truth(value,message) if not value then error(message or "assertion failed",2) end end
function M.run()
  for _,item in ipairs(M.tests) do local ok,value=xpcall(item[2],function(err) return debug.traceback(tostring(err),2) end);if not ok then M.failures[#M.failures+1]=item[1]..": "..value end end
  for _,failure in ipairs(M.failures) do io.stderr:write(failure,"\n") end
  io.stdout:write((#M.tests-#M.failures).." passed, "..#M.failures.." failed\n")
  return #M.failures==0
end
return M
