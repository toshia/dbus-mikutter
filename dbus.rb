# -*- coding: utf-8 -*-
# DBus mikutter plugin
# 2011/01/28 toshi_a

require_if_exist 'rubygems'
require 'dbus'

Module.new do

  module SexpConverter
    def conv_message(message)
      MIKU.unparse([[:id, message[:id]],
                    [:message, message.to_show],
                    [:user, message.idname],
                    [:in_reply_to_status, message[:replyto].to_i],
                    [:in_reply_to_user, message[:reciver].to_i],
                    [:source, message[:source]],
                    [:geo, message[:geo]],
                    [:created, message[:created].to_s]]) end
  end
  extend SexpConverter

  class MikuMikuBus < DBus::Object

    include SexpConverter

    def set_miku(mikuservice)
      @mikuservice = mikuservice
    end

    dbus_interface "org.mikutter.events.timeline" do
      dbus_signal :timeline_update, ":s"
      dbus_method :post, "in contents:s, in replyid:s" do |contents, replyid|
        if @mikuservice
          repmess = nil
          if replyid.to_i != 0
            repmes = Message.findbyid(replyid.to_i)
          end
          @mikuservice.update(:message => contents, :replyto => repmes) { |stat,value|
            # なんもしない
          }
        end
      end
    end
  end

  plugin = Plugin.create(:dbus)
  bus = DBus::SessionBus.instance
  service = bus.request_service("org.mikutter.service")
  b_timeline = MikuMikuBus.new("/org/mikutter/MyInstance")
  service.export(b_timeline)

  plugin.add_event(:boot){ |mikuservice|
    b_timeline.set_miku(mikuservice)
  }

  plugin.add_event(:update){ |service, messages|
    messages.each{ |message|
      b_timeline.timeline_update(conv_message(message).force_encoding('ASCII-8BIT'))
    }
  }

  Thread.new{
    main = DBus::Main.new
    main << bus
    main.run
  }
end
