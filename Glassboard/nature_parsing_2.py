from bs4 import BeautifulSoup
from functools import partial
import re, sys, gc
import os, os.path
from lxml import etree as ET

def get(identifier, tag):
	if tag.has_key('name'):
		if tag['name'] == identifier:
			return True
	return False

def string(tag):
	return tag.string

def is_bib(tag):
	try:
		return 'bib' in tag['id']
	except:
		return False

def process(f):
	p = ET.Element('paper')
	fire = open(f)
	soup = BeautifulSoup(fire)
	dirname = os.path.dirname(f) + '/'
	outdir = dirname.replace('Nature', 'Nature_Processed')
	outfile = os.path.join(outdir, f.replace(dirname, '') + '.xml')
	
	try:
		t = ET.SubElement(p, 'title')
		t.text = soup.find(partial(get, "citation_title"))['content']
		
		dt = ET.SubElement(p, 'date')
		dt.text = soup.find(partial(get, "citation_date"))['content']
		
		d = ET.SubElement(p, 'doi')
		d.text = soup.find(partial(get, "citation_doi"))['content'][4:]
		
		a = ET.SubElement(p, 'authors')
		authors = None
		authors = soup.find_all(partial(get, "DC.creator"))
		if authors == []:
			authors = soup.find_all(partial(get, "dc.creator"))
		if authors == []:
			authors = soup.find(partial(get, "citation_authors"))['content']
			authors = [x.strip() for x in authors.split(',')]
		for aut in authors:
			au = ET.SubElement(a, 'author')
			au.text = aut['content']
	except:
		fire.close()
		total_failures.append(f)
		print "Basics wrong."
		return
	
	k = ET.SubElement(p, 'keywords')
	try:
		keywords = soup.find(partial(get, 'keywords'))['content'].split(',')
		keywords = map(lambda x: x.strip(), keywords)
		for keyword in keywords:
			if 'nature' in keyword:
				continue
			kwd = ET.SubElement(k, 'keyword')
			kwd.text = keyword
	except:
		pass
	
	k2 = ET.SubElement(p, 'article-keywords')
	keywords = soup.find(class_='article-keywords')
	if keywords is None:
		keywords = soup.find(class_="category")
	if keywords is not None:
		for keyword in [x for x in re.split('\s*', keywords.text) if x != '']:
			if ':' in keyword:
				continue
			kdw = ET.SubElement(k2, 'keyword')
			kdw.text = keyword.strip()

	ab = ET.SubElement(p, 'abstract')
	try:
		ab.text = soup.find(id="abs").text
	except:
		try:
			ab.text = soup.find(id="abstract").text
		except:
			ab.text = ''

	refs = soup.find_all(is_bib)
	r_tag = ET.SubElement(p, 'has-references')
	if refs != []:
		r_tag.text = 'Y'
	else:
		r_tag.text = 'N'
	r = ET.SubElement(p, 'references')
	for ref in refs:
		rf = ET.SubElement(r, 'reference')
		rft = ET.SubElement(rf, 'title')
		rfa = ET.SubElement(rf, 'authors')
		rfj = ET.SubElement(rf, 'journal')
		rfy = ET.SubElement(rf, 'year')
		rfd = ET.SubElement(rf, 'doi')
		rfu = ET.SubElement(rf, 'url')
		
		ref_sp = ref.text.strip()
		if ref_sp[0] == '.':
			ref_sp = ref_sp[1:].strip()
		authors_0 = []
		next_index = 0
		try:
			while True:
				if ref_sp[next_index] in [',','.',':',';']:
					authors_0.append(ref_sp[0:next_index])
					if ref_sp[next_index] in ['.',':',';']:
						ref_sp = ref_sp[(next_index + 1):].strip()
						break
					ref_sp = ref_sp[(next_index + 1):].strip()
					next_index = 0
				else:
					next_index += 1
		except:
			failures_0.append(f)
			continue
		# at this point we have removed the names
		journal_0 = ref.find(class_='journal')
		if journal_0 is None:
			failures.append(f)
			continue
		journal_0 = journal_0.text
		ind = ref_sp.find(journal_0)
		title_0 = ref_sp[0:ind]
		ref_sp = ref_sp[ind:]
		mtch = year_regex.search(ref_sp)
		try:
			year_0 = mtch.group(0)[:-1]
		except:
			year_0 = "-1"

		doi_0 = ''
		url_0 = ''
		links = ref.find_all(class_='reftxt')
		for link in links:
			mtch = doi_regex.search(link['href'])
			if mtch is None:
				continue
			else:
				doi_0 = mtch.group(0)
				break
		if doi_0 == '':
			try:
				url_0 = links[0]['href']
			except:
				pass

		rft.text = title_0
		rfj.text = journal_0
		rfy.text = year_0
		rfd.text = doi_0
		rfu.text = url_0
		for a_0 in authors_0:
			if 'et al' in a_0:
				continue
			rfau = ET.SubElement(rfa, 'author')
			rfau.text = a_0

	tree = ET.ElementTree(p)
	if not os.path.exists(outdir):
		os.makedirs(outdir)
	tree.write(outfile, pretty_print=True)
	fire.close()
	soup.decompose()
	tree = soup = None
	gc.collect()

doi_regex = re.compile('10[.][0-9]{3,}(?:[.][0-9]+)*/(?:(?!["&\'<>])\S)+')
year_regex = re.compile('[0-9]{4};')
# mtch = doi_regex.match(string)
# mtch.group(0)
path = '/Users/julienclancy/Desktop/Glassboard/Nature_Processed/nature_journal_new/'

fs = []
for root, dirs, files in os.walk(path):
	for f in files:
		fs.append(os.path.join(root,f))

failures = []
failures_0 = []
total_failures = []
for i in range(len(fs)):
	process(fs[i])
	if i % 1000 == 0:
		print i

date_regex = re.compile('(\d{4,4})')
before_2000 = open('/Users/julienclancy/Desktop/before_2000_nature_2.txt', 'w')
fails = 0
for f in range(len(fs)):
	try:
		root = ET.parse(fs[f]).getroot()
		title = root.find(".//title").text
		date = root.find(".//date").text
		year = int(re.match(date_regex, date).group(0))
		if year < 2000:
			before_2000.write('{' + '{0}'.format(title) + '}\n')
	except:
		fails += 1
		continue
before_2000.close()