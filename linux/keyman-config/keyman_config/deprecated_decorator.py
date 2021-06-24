#!/usr/bin/python3
# based on https://stackoverflow.com/a/40301488
import logging


string_types = (type(b''), type(u''))


def deprecated(reason):
    if isinstance(reason, string_types):
        # The @deprecated is used with a 'reason'.
        def decorator(func1):
            def new_func1(*args, **kwargs):
                logging.warning("Call to deprecated function '{name}': {reason}.".format(
                    name=func1.__name__, reason=reason))
                return func1(*args, **kwargs)
            return new_func1
        return decorator
    else:
        # The @deprecated is used without any 'reason'.
        def new_func2(*args, **kwargs):
            func2 = reason
            logging.warning("Call to deprecated function '{name}'.".format(name=func2.__name__))
            return func2(*args, **kwargs)
        return new_func2
