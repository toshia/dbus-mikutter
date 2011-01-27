# -*- coding: utf-8 -*-
# mikutterからTLを受け取る

require 'rubygems'
require "dbus"

session_bus = DBus::SessionBus.instance

ruby_srv = session_bus.service("org.mikutter.service")

# Get the object from this service
player = ruby_srv.object("/org/mikutter/MyInstance")

# Introspect it
puts player.introspect
player.default_iface = "org.mikutter.events.timeline"
player.on_signal("timeline_update") do |x|
  puts x.to_s
end

main = DBus::Main.new
main << session_bus
main.run
