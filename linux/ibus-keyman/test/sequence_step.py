'''
Base sequence step class and some derivatives.

@author: Eitan Isaacson
@copyright: Copyright (c) 2007 - 2009 Eitan Isaacson
@license: LGPL

http://ldtp.freedesktop.org

This file may be distributed and/or modified under the terms of the GNU Lesser General
Public License version 2 as published by the Free Software Foundation. This file
is distributed without any warranty; without even the implied warranty of 
merchantability or fitness for a particular purpose.

See "COPYING" in the source distribution for more information.

Headers in this file shall remain intact.
'''

import pyatspi, sys
try:
  from gi.repository import GObject as gobject
except:
  import gobject

class SequenceStep(gobject.GObject):
  '''
  Base class for all sequence steps in a Macaroon sequence. Emits a "done" 
  signal when the step is done.

  @cvar delta_time: Time, in milliseconds, before this step should be executed.
  @type delta_time: integer

  @ivar done: True if step is done.
  @type done: boolean
  '''
  __gsignals__ = {'done' : (gobject.SIGNAL_RUN_FIRST, 
                                 gobject.TYPE_NONE, ())}
  delta_time = 0
  def __init__(self):
    '''
    Initialize L{SequenceStep}.
    '''
    try:
      super(SequenceStep, self).__init__()
    except:
      self.__gobject_init__()
    self.done = False

  def stepDone(self):
    '''
    Puts instance in "done" state. And emits "done" signal.
    
    @return: Return False because this is usually called through a 
    gobject timeout.
    @rtype: boolean
    '''
    if not self.done:
      self.done = True
      self.emit('done')
    return False

class AtomicAction(SequenceStep):
  '''
  A L{SequenceStep} that performs a certain action.

  @ivar _func: Function to call.
  @type _func: callable
  @ivar _args: Arguments to give the function.
  @type _args: list
  @ivar _kwargs: Key word arguments for function.
  @type _kwargs: dictionary
  '''
  def __init__(self, delta_time, func, *args, **kwargs):
    '''
    Initialize L{AtomicAction}.
    
    @param delta_time: Time, in milliseconds, before this step should be 
    executed.
    @type delta_time: integer
    @param func: Function to call.
    @type func: callable
    @param _args: Arguments to give the function.
    @type _args: list
    @param _kwargs: Key word arguments for function.
    @type _kwargs: dictionary
    '''
    SequenceStep.__init__(self)
    self.delta_time = delta_time
    self._func = func
    self._args = args
    self._kwargs = kwargs

  def __call__(self):
    '''
    Perform the given function.
    '''
    self._func(*self._args, **self._kwargs)
    self.stepDone()

class DebugAction(AtomicAction):
  '''
  An action that prints a debug message to standard output.
  '''
  def __init__(self, debug_msg, delta_time=0):
    '''
    Initialize L{DebugAction}
    
    @param debug_msg: Message to print out to standard output.
    @type debug_msg: string
    @param delta_time: Time to wait before printing the message.
    @type delta_time: integer
    '''
    self._debug_msg = debug_msg
    AtomicAction.__init__(self, 0, self._printDebugMsg)

  def _printDebugMsg(self):
    '''
    Print the debug message.
    '''
    print(self._debug_msg)

  def __str__(self):
    '''
    String representation of instance.

    @return: String representation of instance.
    @rtype: string
    '''
    return 'Debug message: %s' % self._debug_msg

class PauseAction(AtomicAction):
  '''
  A simple pause for a given amount of time.
  '''
  def __init__(self, delta_time):
    '''
    Initialize a L{PauseAction}.
    
    @param delta_time: Duration of pause in milliseconds.
    @type delta_time: integer
    '''
    AtomicAction.__init__(self, delta_time, lambda args, kwargs: None, [], {})

  def __str__(self):
    '''
    String representation of instance.
    
    @return: String representation of instance.
    @rtype: string
    '''
    return 'Pause for %d milliseconds' % self.delta_time


class CallableAction(AtomicAction):
  '''
  A simplified L{AtomicAction} that makes it easy for simply calling a function.
  '''
  def __init__(self, func, *args, **kwargs):
    '''
    Initialize L{AtomicAction}.
    
    @param delta_time: Time, in milliseconds, before this step should be 
    executed.
    @type delta_time: integer
    @param func: Function to call.
    @type func: callable
    @param _args: Arguments to give the function.
    @type _args: list
    @param _kwargs: Key word arguments for function.
    @type _kwargs: dictionary
    '''
    AtomicAction.__init__(self, 0, func, *args, **kwargs)
