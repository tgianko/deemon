from urlparse import urlparse, parse_qs
from cStringIO import StringIO
import multipart

class Node:
	def __init__(self):
		self.id = None

	def set_id(self, id):
		self.id = id

	def __create_node__(self):
		return ""


class Edge:
	def __create_rel__(self):
		return ""

class Custom(Node):
	def __init__(self, ntype, value):
		Node.__init__(self)
		self.value = value
		self.ntype = ntype

	def __create_node__(self):
		return """({id}:{ntype} {{ value: "{v}"}})""".format(
				id   = self.id, 
				ntype = self.ntype,
				v    = self.value)


class String(Node):
	def __init__(self, value):
		Node.__init__(self)
		self.value = value

	def __create_node__(self):
		return """({id}:String {{ value: "{v}"}})""".format(
				id = self.id, 
				v  = self.value)


class Pair(Node, Edge):
	def __init__(self, k, v, ntype):
		self.k = String(k)
		self.v = String(v)
		self.ntype = ntype

	def set_id(self, id):
		self.id = id
		self.k.set_id("{}_1".format(id, 1))
		self.v.set_id("{}_2".format(id, 2))

	def __create_node__(self):
		return """({id}:{ntype}), {k}, {v}""".format(
				id = self.id, 
				k  = self.k.__create_node__(), 
				v  = self.v.__create_node__(),
				ntype=self.ntype)

	def __create_rel__(self):
		return """({id})-[:Key]->({kid}), ({id})-[:Value]->({vid})""".format(id=self.id, kid=self.k.id, vid=self.v.id)

class ParameterList(Node, Edge):
	def __init__(self, d, ntype):
		self._data = []
		for k, vs in d.items():
			if type(vs) in [iter, tuple]:
				for v in vs:
					self._data.append(Pair(k, v, "Parameter"))
			else:
				self._data.append(Pair(k, vs, "Parameter"))
		self.ntype = ntype

	def set_id(self, id):
		self.id = id
		for p in self._data:
			p.set_id("{}_{}".format(id, self._data.index(p)))

	def __create_node__(self):
		return """({id}:{ntype}), {rest}""".format(
				id   = self.id, 
				ntype = self.ntype,
				rest = ", ".join([p.__create_node__() for p in self._data]))

	def __create_rel__(self):
		return """{p1}, {p2}""".format(
				p1=", ".join(["""({id})-[:Parameter]->({par})""".format(id=self.id, par=p.id) for p in self._data]), 
				p2=", ".join([p.__create_rel__() for p in self._data]))

class URL(Node, Edge):
	def __init__(self, url):
		scheme, netloc, path, params, query, fragment = urlparse(url)
		self.scheme   = String(scheme)
		self.netloc   = String(netloc)
		
		if len(path) > 0:
			self.path = String(path)
		else:
			self.path = None

		if len(params) > 0:
			self.params   = String(params)
		else:
			self.params = None

		if len(query) > 0:
			self.query    = ParameterList(parse_qs(query), "QueryString")
		else:
			self.query = None

		if len(fragment) > 0:
			self.fragment = String(fragment)
		else:
			self.fragment = None

	def set_id(self, id):
		self.id = id
		self.scheme.set_id("scheme")
		self.netloc.set_id("domain")

		if self.path:
			self.path.set_id("path")

		if self.params:
			self.params.set_id("param")

		if self.query:
			self.query.set_id("query")

		if self.fragment:
			self.fragment.set_id("fragment")

	def __create_node__(self):

		s = """({id}:URL), {scheme}, {netloc}""".format(
			id      = self.id, 
			scheme  = self.scheme.__create_node__(), 
			netloc  = self.netloc.__create_node__())

		if self.path:
			s += """, {path}""".format(
				path    = self.path.__create_node__())

		if self.params:
			s += """, {params}""".format(
				params  = self.params.__create_node__())

		if self.query:
			s += """, {query}""".format(
				query   = self.query.__create_node__())

		if self.fragment:
			s += """, {fragment}""".format(
				fragment= self.fragment.__create_node__())

		return s

	def __create_rel__(self):
		s = """({id})-[:Scheme]->({scheme}), ({id})-[:Netloc]->({netloc})""".format(
				id       = self.id, 
				scheme   = self.scheme.id, 
				netloc   = self.netloc.id)

		if self.path:
			s += """, ({id})-[:Path]->({path})""".format(
				id       = self.id,
				path     = self.path.id)

		if self.params:
			s += """, ({id})-[:Params]->({params})""".format(
				id       = self.id, 
				params   = self.params.id)

		if self.query:
			s += """, ({id})-[:Query]->({query}), {rest}""".format(
				id       = self.id,
				query    = self.query.id,
				rest     = self.query.__create_rel__() )

		if self.fragment:
			s += """, ({id})-[:Fragment]->({fragment})""".format(
				id       = self.id, 
				fragment = self.fragment.id)

		return s

r = (1, 0, u'2016-08-23 16:11:39.432159', u'https://192.168.56.101/', None, u'accept-language: en-US,en;q=0.5\r\naccept-encoding: gzip, deflate, br\r\nhost: 192.168.56.101\r\naccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nuser-agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:48.0) Gecko/20100101 Firefox/48.0\r\nupgrade-insecure-requests: 1', u'GET', u'', u'200')
r = (17, 19, u'2016-08-23 16:14:38.020004', u'https://192.168.56.101/index.php?rt=account/create', u'-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="loginname"\r\n\r\ncrashtest\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="firstname"\r\n\r\nCrash\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="lastname"\r\n\r\nTest\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="email"\r\n\r\ncrashtest@gmail.com\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="telephone"\r\n\r\n00491234567\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="fax"\r\n\r\n00491234567\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="company"\r\n\r\nSaarland University\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="address_1"\r\n\r\nCampus\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="address_2"\r\n\r\n\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="city"\r\n\r\nSaarbruecken\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="postcode"\r\n\r\n66123\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="country_id"\r\n\r\n81\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="zone_id"\r\n\r\n1265\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="password"\r\n\r\n123456\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="confirm"\r\n\r\n123456\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="newsletter"\r\n\r\n0\r\n-----------------------------18325766051414425461215726253\r\nContent-Disposition: form-data; name="agree"\r\n\r\n1\r\n-----------------------------18325766051414425461215726253--\r\n', u'content-length: 2095\r\naccept-language: en-US,en;q=0.5\r\naccept-encoding: gzip, deflate, br\r\naccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nuser-agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:48.0) Gecko/20100101 Firefox/48.0\r\nhost: 192.168.56.101\r\nreferer: https://192.168.56.101/index.php?rt=account/create\r\ncookie: AC_SF_80F8C5D7B2=2ctfes5cltjguac2cr4ves7hr5; language=en; currency=USD\r\nupgrade-insecure-requests: 1\r\ncontent-type: multipart/form-data; boundary=---------------------------18325766051414425461215726253', u'POST', u'AC_SF_80F8C5D7B2     2ctfes5cltjguac2cr4ves7hr5\nlanguage             en\ncurrency             USD', u'302')

class Request(Node, Edge):
	def __init__(self, method, url, hdrs, body, pos=-1, ts=-1):
		# Parsing URL
		self.method = String(method)
		
		# Parsing URL
		self.url = URL(url)
		
		if hdrs[-2:] == "\r\n":
			hdrs = hdrs[:-2]
		
		# Parsing headers
		d = {}
		for hdr in hdrs.split("\r\n"):
			if len(hdr) == 0:
				continue
			k, v = hdr.split(":", 1)
			v = v.strip()
			d[k] = v
		self.hdrs = ParameterList(d, "HeaderList")

		# Parsing body
		self.body = None
		for hdr in hdrs.split("\r\n"):
			if len(hdr) == 0:
				continue
			k, v = hdr.split(":", 1)
			v = v.strip()
			if k.lower() == "content-type":
				if "multipart/form-data" in v:

					# parse multipart/form-data body
					s_obj = StringIO(body)
					boundary = v.split("; boundary=")[1]
					mp = multipart.MultipartParser(s_obj, boundary)
					d = {}
					for part in mp:
						k = part.options.get("name")
						v = part.value
						d[k] = v
					self.body = ParameterList(d, "MultipartParameters")

				elif "application/x-www-form-urlencoded" in v:

					# parse application/x-www-form-urlencoded body
					self.body = ParameterList(parse_qs(body), "XWWWForm")

				else:
					# No JSON yet...
					self.body = String(body)
				break

		self.pos = pos
		self.ts  = ts

	def set_id(self, id):
		self.id = id
		self.method.set_id("{}_method".format(id))
		self.url.set_id("{}_url".format(id))
		self.hdrs.set_id("{}_hdrs".format(id))
		if self.body:
			self.body.set_id("{}_hdrs".format(id))

	def __create_node__(self):
		s = """({id}:HTTPRequest {{ ts: {ts}, pos: {pos} }}), {method}, {url}, {hdrs}""".format(
				id     = self.id,
				ts     = self.ts,
				pos    = self.pos,
				method = self.method.__create_node__(),
				url    = self.url.__create_node__(),
				hdrs   = self.hdrs.__create_node__()
			)
		if self.body:
			s += """, {body}""".format(
				body   = self.body.__create_node__())

		return s
		

	def __create_edge__(self):
		return """({id})-[:Method]->({method}), ({id})-[:URL]->({url}), ({id})-[:Headers]->({hdrs}), ({id})-[:Body]->({body}), {p1}, {p2}, {p3}""".format(
				id     = self.id,
				method = self.method.id,
				url    = self.url.id,
				hdrs   = self.hdrs.id,
				body   = self.body.id,
				p1     = self.url.__create_rel__(),
				p2     = self.hdrs.__create_rel__(),
				p3     = self.body.__create_rel__()
			)

