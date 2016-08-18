function read_query(packet)
   if string.byte(packet) == proxy.COM_QUERY then
     query = string.sub(packet, 2)
     log_query(query)
   end
 end

function log_query(query)
   output = os.time() .. ";QUERY;" .. query
   io.stdout:setvbuf("no")
   print(output)
end
