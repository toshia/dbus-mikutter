# -*- coding: utf-8 -*-
# DBus mikutter plugin
# 2011/01/28 toshi_a

require_if_exist 'rubygems'
require_if_exist 'dbus'

if defined?(DBus::Object)
  Module.new do

    class Message
      def to_i
        self[:id].to_i end end

    class MikuMikuBus < DBus::Object
      dbus_interface "org.mikutter.events.timeline" do
        dbus_signal :timeline_update, ":s"
      end
    end

    plugin = Plugin.create(:dbus)
    bus = DBus::SessionBus.instance
    service = bus.request_service("org.mikutter.service")
    b_timeline = MikuMikuBus.new("/org/mikutter/MyInstance")
    service.export(b_timeline)

    plugin.add_event(:update){ |service, messages|
      messages.each{ |message|
        b_timeline.timeline_update(MIKU.unparse([[:id, message[:id]],
                                                 [:message, message.to_show],
                                                 [:user, message.idname],
                                                 [:in_reply_to_status, message[:replyto].to_i]]))
      }
    }

    Thread.new{
      main = DBus::Main.new
      main << bus
      main.run
    }
  end
end

