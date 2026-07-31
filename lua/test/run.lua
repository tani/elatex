local source=debug.getinfo(1,"S").source:sub(2):gsub("/test/run%.lua$","")
package.path=source.."/?.lua;"..source.."/?/init.lua;"..package.path
local harness=require("test.harness")
require("test.api_test")
if arg[1]=="--oracle" then local oracle=os.getenv("ELATEX_ORACLE");assert(oracle and oracle~="","--oracle requires ELATEX_ORACLE");assert(require("test.differential_test")) else require("test.golden_test") end
os.exit(harness.run() and 0 or 1)
