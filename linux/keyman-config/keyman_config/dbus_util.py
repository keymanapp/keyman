#!/usr/bin/python3
import logging
import os

import dbus
import dbus.bus
import dbus.mainloop.glib
import dbus.service

BUS_NAME = 'com.Keyman.Config'
OBJECT_PATH = '/com/Keyman/Config'


__keyman_config_service = None


class KeymanConfigService(dbus.service.Object):
    def __init__(self, bus, handler) -> None:
        self.__keyboard_list_changed_handler = handler
        self.__bus = bus
        bus_name = dbus.service.BusName(BUS_NAME, bus=self.__bus, do_not_queue=True)
        dbus.service.Object.__init__(self, bus_name, OBJECT_PATH)

    @dbus.service.signal(BUS_NAME)
    def KeyboardListChangedSignal(self):
        pass

    @dbus.service.method(BUS_NAME)
    def keyboard_list_changed(self):
        logging.debug("%s: -------------keyboard_list_changed--------------------" % os.getpid())
        if self.__keyboard_list_changed_handler:
            self.__keyboard_list_changed_handler()
        self.KeyboardListChangedSignal()


class KeymanConfigServiceManager:
    def __init__(self, handler):
        self.__name_owner_watch = None
        self.__signal_receiver = None
        self.__service = None
        self.__keyboard_list_changed_handler = handler
        loop = dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)
        self.__bus = dbus.SessionBus(loop)
        self.__name_owner_watch = self.__bus.watch_name_owner(BUS_NAME, self.__name_owner_changed)

    def __is_name_owner(self) -> bool:
        retval = self.__bus.request_name(BUS_NAME, dbus.bus.NAME_FLAG_DO_NOT_QUEUE)
        return (
          retval == dbus.bus.REQUEST_NAME_REPLY_ALREADY_OWNER or
          retval == dbus.bus.REQUEST_NAME_REPLY_PRIMARY_OWNER)

    def __create_service(self) -> None:
        if self.__signal_receiver:
            self.__signal_receiver.remove()
            self.__signal_receiver = None
        if self.__is_name_owner():
            logging.debug('%s: I am now owner (%s)' % (os.getpid(), self.__bus.get_name_owner(BUS_NAME)))
            if self.__name_owner_watch:
                self.__name_owner_watch.cancel()
                self.__name_owner_watch = None
            self.__service = KeymanConfigService(self.__bus, self.__keyboard_list_changed_handler)
        else:
            logging.debug('%s: Not owner. Owner is %s. Connecting signal' %
                          (os.getpid(), self.__bus.get_name_owner(BUS_NAME)))
            self.__service = self.__bus.get_object(BUS_NAME, OBJECT_PATH, introspect=False)
            if self.__keyboard_list_changed_handler:
                self.__signal_receiver = self.__service.connect_to_signal(
                  'KeyboardListChangedSignal', self.__keyboard_list_changed_handler, dbus_interface=BUS_NAME)

    def __name_owner_changed(self, new_owner) -> None:
        logging.debug('%s: Owner changed. Recreating service.' % os.getpid())
        self.__create_service()

    def __verify_service_exists(self) -> None:
        if not self.__service:
            self.__create_service()

    def keyboard_list_changed(self) -> None:
        self.__verify_service_exists()
        if self.__service:
            self.__service.keyboard_list_changed()


def get_keyman_config_service(handler=None) -> KeymanConfigServiceManager:
    global __keyman_config_service
    if not __keyman_config_service:
        __keyman_config_service = KeymanConfigServiceManager(handler)
    return __keyman_config_service
