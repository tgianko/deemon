from lxml import html
from os.path import join, dirname

class SeleneseCommand(object):
    def __init__(self, columns):
        self.columns = columns
        self._command = None
        self._target = None
        self._value = None

    def command(self):
        if not self._command:
            self._command = self.columns[0].text or ''
            self._command = self._command.strip()
        return self._command

    def target(self):
        if not self._target:
            self._target = self.columns[1].text or ''
            self._target = self._target.strip()
        return self._target

    def value(self):
        if not self._value:
            self._value = self.columns[2].text or ''
            self._value = self._value.strip()
        return self._value

class SeleneseTestCase(object):
    def __init__(self, filename):
        self.tree = html.parse(filename).getroot()
        self._name = None

    def __iter__(self):
        for row in self.tree.xpath('//tr'):
            columns = row.xpath('td')
            if len(columns) == 3:
                yield SeleneseCommand(columns)

    def name(self):
        if not self._name:
            self._name = self.tree.xpath('//thead//td')[0].text.strip()
        return self._name

    def baseurl(self):
        if not hasattr(self, '_baseurl'):
            element = self.tree.xpath('//link[@rel="selenium.base"]')
            if len(element) > 0:
                self._baseurl = element[0].attrib['href']
            else:
                self._baseurl = None
        return self._baseurl



class SeleneseTestSuite(object):
    
    def __init__(self, filename):
        self.dirname = dirname(filename)
        self.tree = html.parse(filename).getroot()
        self._name = None

    def __iter__(self):
        for a in self.tree.xpath('//tr//td//a'):
            yield SeleneseTestCase(join(self.dirname, a.attrib["href"]))



def is_suite(filename):
    tree = html.parse(filename)
    if tree.xpath('//title')[0].text == "Test Suite":
        return True
    return False
