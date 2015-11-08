#!/usr/bin/ruby
require 'nokogiri'

################################################################
## This is a simple example of accessing cb2xml from ruby
## It simply prints the content of the Xml
## Author: Bruce Martin
## License: Any, e.g. LGPL (any), Apache, Creative Commons
################################################################
   
def printXml indent, tree

    tree.each do |node|
    	 ##next if !node["position"].nil?

    	 puts "#{indent} #{node["level"]} #{node["name"]}\t#{node["position"]}\t#{node["storage-length"]}\t#{node["picture"]}\t#{node["usage"]}"   
    	 printXml "#{indent}   ", node.children   
    end
end

   doc = Nokogiri.XML(File.open("cb2xml_Output110.xml"))
   
   printXml "    ", doc.root.children
