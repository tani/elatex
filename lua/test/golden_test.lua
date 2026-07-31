local harness=require("test.harness");local fixtures=require("test.fixtures");local elatex=require("elatex")
for _,entry in ipairs(fixtures.all()) do
  local name=entry.file.."/b"..string.format("%03d",entry.block_index).."/r"..string.format("%03d",entry.reference_index).."/a"..string.format("%03d",entry.alternative_index)
  harness.test(name,function() harness.equal(elatex.render(entry.input,fixtures.options(entry)).output,entry.expected) end)
end
