import pathlib
from typing import NamedTuple, NewType, Tuple, List, Union
from enum import Enum
from collections.abc import Sequence, Mapping

USV = int
Marker = int
VirtualKey = int


class Context(Sequence):
    Item = Union[USV, Marker]

    def __init__(initial: str):
        pass

    def __del__(self):
        pass

    def __str__(self):
        pass

    def set(self, ctxt: List[Item]):
        self.clear()
        self.apped(ctxt)

    def clear(self):
        pass

    def __getitem__(self, key: int) -> Item:
        pass

    def __len__(self):
        pass

    def append(self, ctxt: List[Item]):
        pass

    def delete(self, remove_n: int, prefix: List[Item]):
        pass


Option = Tuple[str, str]


class OptionSet(Mapping):
    def __getitem__(self, key: str) -> Option:
        pass

    def __iter__(self):
        pass

    def __len__(self):
        pass

    def __str__(self):
        pass


class Keyboard(NamedTuple('__kb_attrs', version=str, id=str, folder_path=pathlib.Path, default_options=OptionSet)):
    def __new__(cls, _):
        return super(Keyboard, cls).__new__(cls, *[None]*4)

    def __init__(self, kb_path: pathlib.Path):
        pass

    def __del__(self):
        pass


class Action:
    VKeyDown = NewType('Action.VirtualKey', VirtualKey)
    # VKeyUp = VirtualKey
    # VShiftDown = VirtualKey
    # VShiftUp = VirtualKey
    # Char = int
    # Marker = int
    # Bell = NewType('Bell', None)
    # Back = NewType('Back', None)
    # PersistOpt = str
    # ResetOpt = str


ActionList = List[Action]


class State:
    def __init__(self, kb: Keyboard, env: OptionSet):
        pass

    def __del__(self):
        pass

    @property
    def flags(self) -> int:
        pass

    @property
    def context(self) -> Context:
        pass

    @property
    def environment(self) -> OptionSet:
        pass

    @property
    def options(self) -> OptionSet:
        pass

    def indentify_option_src(opt: Option):
        pass


def process_event(vk: VirtualKey,
                  modifier_state,
                  state: State,
                  acts: ActionList):
    pass
