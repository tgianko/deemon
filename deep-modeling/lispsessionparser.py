"""
Greenspun's tenth law is partially
implemented in this file
"""
from string import lstrip
from neo4jmodel.ApplicationDataLevelSession import SessionElementArray,\
    SessionElementEmpty, SessionElementInteger, SessionElementString,\
    SessionElementType, PHPSession, SessionContent


def skipLeadingBlank(string):
    return lstrip(string)


def skipToNextStringElement(string, element):
    return string[string.index(element):]


def parseSessionContentElementArrayContent(string):
    assert(string[0] == '(')
    rem = string[1:]
    content = dict()
    while(rem[0] != ')'):
        arrayElemIdent, rem = parseSessionContentElement(rem)
        assert(rem[0] == ' ')
        assert(rem[1] == '=')
        assert(rem[2] == '>')
        assert(rem[3] == ' ')
        rem = rem[4:]
        arrayElemContent, rem = parseSessionContentElement(rem)
        content[arrayElemIdent.getValue()] = arrayElemContent
    rem = rem[1:]
    return content, rem


def stringToType(string):
    if string == ":INTEGER":
        return SessionElementType.integer
    elif string == ":STRING":
        return SessionElementType.string
    elif string == ":ARRAY":
        return SessionElementType.array
    elif string == ":NIL":
        return SessionElementType.empty
    else:
        print "Unknown Session Element Type {}".format(string)
        raise Exception
        

def parseSessionContentElementType(string):
    return [stringToType(string[0:string.index(' ')]),
            string[string.index(' ') + 1:]]
    

def parseSessionContentElement(string):
    string = skipLeadingBlank(string)
    assert string[0] == '(', "expect leading ( for session element"
    type, rem = parseSessionContentElementType(string[1:])
    rem = skipLeadingBlank(rem)
    assert(rem[0] == '.')
    assert(rem[1] == ' ')
    if type == SessionElementType.empty:
        assert(rem[2] == 'N')
        assert(rem[3] == ')')
        return SessionElementEmpty(), rem[4:]
    elif type == SessionElementType.string:
        element = SessionElementString(rem[2:rem.index(')')])
        rem = rem[rem.index(')')+1:]
        return element, rem
    elif type == SessionElementType.integer:
        integerContent = rem[2:rem.index(')')]
        rem = rem[rem.index(')')+1:]
        return [SessionElementInteger(integerContent),
                rem]
    elif type == SessionElementType.array:
        arrayElements, rem = parseSessionContentElementArrayContent(rem[2:])
        rem = skipLeadingBlank(rem)
        assert(rem[0] == ')')
        return [SessionElementArray(arrayElements),
                rem[1:]]
    else:
        raise "we should never end up here as it is exhaustive match"


def parseSessionContentIdent(string):
    name = string[0:string.index(' ')]
    rem = string[string.index(' '):]
    return name, rem


def parseSessionContent(string):
    assert(string[0] == '(')
    rem = string
    name, rem = parseSessionContentIdent(string[1:])
    rem = skipLeadingBlank(rem)
    element, rem = parseSessionContentElement(rem)
    rem = skipLeadingBlank(rem)
    return SessionContent(name, element), rem[1:]


def parseSessionName(string):
    return string[1:string.index(' ')], string[string.index(' '):]


def parseSession(string):
    name, rem = parseSessionName(string)
    rem = skipLeadingBlank(rem)
    contentList = list()
    assert(rem[0] == '(')
    rem = skipLeadingBlank(rem[1:])
    assert(rem[0] == '(')
    while(rem[0] != ')'):
        rem = skipLeadingBlank(rem)
        content, rem = parseSessionContent(rem)
        contentList.append(content)
        rem = skipLeadingBlank(rem)
    assert(rem[2:] == '')
    return PHPSession(name, contentList)

