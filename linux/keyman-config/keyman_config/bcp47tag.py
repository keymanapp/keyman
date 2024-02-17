#!/usr/bin/python3
import re


class Bcp47Tag():
    def __init__(self, language=None, script=None, region=None, privateUse=[], variant=[],
                 extension=[], grandFathered=None, extLang=[], langTagPrivateUse=[]):
        self.language = language
        self.privateUse = privateUse
        self.variant = variant
        self.extension = extension
        self.script = script
        self.grandFathered = grandFathered
        self.region = region
        self.extLang = extLang
        self.langTagPrivateUse = langTagPrivateUse
        self.__originalTag = None

    def __eq__(self, other):
        if not isinstance(other, Bcp47Tag):
            # don't attempt to compare against unrelated types
            return NotImplemented

        return self.language == other.language and self.privateUse == other.privateUse and self.variant == other.variant and self.extension == other.extension and self.script == other.script and self.grandFathered == other.grandFathered and self.region == other.region and self.extLang == other.extLang and self.langTagPrivateUse == other.langTagPrivateUse

    def __str__(self):
        return "%s|%s|%s|%s|%s|%s|%s|%s|%s" % (self.language, self.extLang, self.script,
                                               self.region, self.variant, self.privateUse,
                                               self.langTagPrivateUse, self.grandFathered,
                                               self.extension)

    def __repr__(self):
        return "%s (0x%x)" % (self.tag, id(self))

    @property
    def originalTag(self):
        return self.__originalTag

    @property
    def tag(self):
        if self.grandFathered:
            return self.grandFathered
        if self.privateUse:
            return 'x-' + '-'.join(self.privateUse)

        result = self.language
        if self.extLang:
            result += '-' + '-'.join(self.extLang)
        if self.script:
            result += '-' + self.script
        if self.region:
            result += '-' + self.region
        if self.variant:
            result += '-' + '-'.join(self.variant)
        if self.extension:
            result += '-' + '-'.join(self.extension)
        if self.langTagPrivateUse:
            result += '-' + '-'.join(self.langTagPrivateUse)
        return result

    @tag.setter
    def tag(self, value):
        self._processTag(value)
        return

    @staticmethod
    def create(tag):
        bcp47Tag = Bcp47Tag()
        return bcp47Tag._processTag(tag)

    def _processTag(self, tag):
        # Adapted from https://github.com/gagle/node-bcp47 [MIT]
        regex = re.compile(r'^(?:(en-GB-oed|i-ami|i-bnn|i-default|i-enochian|i-hak|i-klingon|' +
                           r'i-lux|i-mingo|i-navajo|i-pwn|i-tao|i-tay|i-tsu|' +
                           r'sgn-BE-FR|sgn-BE-NL|sgn-CH-DE)|(art-lojban|cel-gaulish|' +
                           r'no-bok|no-nyn|zh-guoyu|zh-hakka|zh-min|zh-min-nan|zh-xiang))$' +
                           r'|^((?:[a-z]{2,3}(?:(?:-[a-z]{3}){1,3})?)|[a-z]{4}|' +
                           r'[a-z]{5,8})(?:-([a-z]{4}))?(?:-([a-z]{2}|\d{3}))?((?:-(?:[\da-z]{5,8}' +
                           r'|\d[\da-z]{3}))*)?((?:-[\da-wy-z](?:-[\da-z]{2,8})+)*)?(-x(?:-[\da-z]{1,8})+)?$|^(x(?:-[\da-z]{1,8})+)$', re.IGNORECASE)
        match = regex.match(tag)
        if not match:
            return None

        self.__originalTag = tag

        self.grandFathered = None
        self.language = None
        self.extLang = []
        self.script = None
        self.region = None
        self.variant = []
        self.extension = []
        self.langTagPrivateUse = []
        self.privateUse = []

        if match[1]:
            self.grandFathered = match[1]
        elif match[2]:
            self.grandFathered = match[2]

        # langtag language
        if match[3]:
            t = match[3].split('-')
            self.language = t.pop(0).lower()
            self.extLang = t

        if match[4]:
            self.script = match[4].title()
        if match[5]:
            self.region = match[5].upper()

        # langtag variant
        if match[6]:
            self.variant = match[6].split('-')
            self.variant.pop(0)

        # langtag extension
        if match[7]:
            t = match[7].split('-')
            t.pop(0)

            singleton = None
            ext = []

            while len(t):
                e = t.pop(0)
                if len(e) == 1:
                    if singleton:
                        self.extension.append("%s-%s" % (singleton, '-'.join(ext)))
                        if (len(list(filter(lambda x: x.startswith(e), self.extension))) > 0):
                            return None
                        singleton = e
                        ext = []
                    else:
                        singleton = e
                else:
                    ext.append(e)
            self.extension.append("%s-%s" % (singleton, '-'.join(ext)))

        # langtag privateuse
        if match[8]:
            self.langTagPrivateUse = match[8].split('-')
            self.langTagPrivateUse.pop(0)

        # privateuse
        if match[9]:
            self.privateUse = match[9].split('-')
            self.privateUse.pop(0)

        return self
