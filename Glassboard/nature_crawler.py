import requests, re, os, os.path, time, unicodedata, datetime
from bs4 import BeautifulSoup, SoupStrainer
from random import choice
from threading import Thread
from TorCtl import TorCtl

too_far = ['error: page not found', '404 error']
base_path = '/Users/julienclancy/Desktop/Glassboard/'
out_path = base_path + 'Crawling/Nature/Nature_HTML/'
# this is used for maintaining an up-to-date database without double crawls. do not modify this file.
last_file = base_path + 'Crawling/Nature/last.txt'
base_url = 'http://www.nature.com/{0}/journal/v{1}/n{2}/index.html'
format_regex = re.compile('([a-z]*?): (\d*?) (\d*?)\n')
# the list of journals that we want to crawl; nature is not in here, as we'll see below
journals = ['aja',
 'ajg',
 'am',
 'aps',
 'bcj',
 'bdj',
 'bjc',
 'bmt',
 'cdd',
 'cddis',
 'cgt',
 'clpt',
 'cmi',
 'cr',
 'ctg',
 'cti',
 'ebd',
 'ejcn',
 'ejhg',
 'emboj',
 'embor',
 'emi',
 'emm',
 'eye',
 'gene',
 'gim',
 'gt',
 'hdy',
 'hr',
 'icb',
 'ijir',
 'ijo',
 'ijos',
 'ismej',
 'ja',
 'jcbfm',
 'jes',
 'jhg',
 'jhh',
 'jid',
 'jidsp',
 'jp',
 'ki',
 'labinvest',
 'leu',
 'lsa',
 'mi',
 'modpathol',
 'mp',
 'mt',
 'mtna',
 'nbt',
 'ncb',
 'nchem',
 'nchembio',
 'neuro',
 'ng',
 'ngeo',
 'ni',
 'nm',
 'nmat',
 'nmeth',
 'nnano',
 'nphoton',
 'nphys',
 'npp',
 'nprot',
 'nrc',
 'nrcardio',
 'nrclinonc',
 'nrd',
 'nrendo',
 'nrg',
 'nrgastro',
 'nri',
 'nrm',
 'nrmicro',
 'nrn',
 'nrneph',
 'nrneurol',
 'nrrheum',
 'nrurol',
 'nsmb',
 'nutd',
 'onc',
 'oncsis',
 'pcan',
 'pj',
 'pr',
 'psp',
 'sc',
 'scientificamerican',
 'tp',
 'tpj',
 'vital']
# this requires a list of user-agents, if you want one
with open(base_path + 'Crawling/user_agents.txt', 'r') as f:
	user_agents = f.read().split('\n')[:-1]

# OPERATION INSTRUCTIONS
# 1. Run initialize_last_crawled()
# 2. Every so often, run update()
# 3. That's it. Make sure to look through the constants and change them to your liking first.

# everything here works for everything but nature itself
def issue_exists(page):
	return too_far[0] not in page.lower() and too_far[1] not in page.lower()

def to_ascii(s):
	return unicodedata.normalize('NFKD', s).encode('ascii', 'ignore')

def is_fulltext(t):
	return 'full text' in t.lower() or 'fulltext' in t.lower()

def is_abstract(t):
	return 'abstract' in t.lower() or 'reference' in t.lower()

# returns volume, assumes that the first available issue is the first of some volume
def first_available(journal):
	print "started working on journal {0}".format(journal)
	vol = 0
	iss = 1
	page = 'Error: page not found'
	while not issue_exists(page):
		vol += 1
		url = base_url.format(journal, str(vol), '1')
		hdr = {'User-Agent': choice(user_agents)}
		page = requests.get(url, headers=hdr).text
		if vol > 30:
			print "check out journal {0}".format(journal)
	return vol

# returns a tuple of (volume, issue), the last available volume and issue
def last_available(journal, vol_start):
	vol = vol_start
	page = ''
	while issue_exists(page):
		vol += 1
		url = base_url.format(journal, str(vol), '1')
		hdr = {'User-Agent': choice(user_agents)}
		page = requests.get(url, header=hdr).text
	vol -= 1
	iss = 0
	page = ''
	while issue_exists(page):
		iss += 1
		url = base_url.format(journal, str(vol), str(iss))
		hdr = {'User-Agent': choice(user_agents)}
		page = requests.get(url, header=hdr).text
	iss -= 1
	return (vol, iss)

# processes a single article given a URL; does not parse
def process_article(url, journal):
	try:
		hdr = {'User-Agent': choice(user_agents)}
		req = requests.get(url, headers=hdr)
		soup = BeautifulSoup(req.content, parse_only=SoupStrainer('meta'))
		section = soup.find('', {'name':'prism.section'})['content']
		doi_tag = soup.find('', {'name':'citatiton_doi'})
		doi_tag = soup.find('', {'name':'dc.identifier'}) if doi_tag is None else doi_tag
		doi_tag = soup.find('', {'name':'DC.identifier'}) if doi_tag is None else doi_tag
		doi = doi_tag['content'].replace('doi:', '')
		save_path = out_path + journal + '/' + section + '/'
		if not os.path.exists(save_path):
			os.makedirs(save_path)
		file_path = save_path + doi.replace('/', '>>') + '.html'
		with open(file_path, 'w') as f:
			f.write(req.content)
		return 0
	except:
		print "WRONG"
		return 1

# gets an entire issue of a given journal and volume
def get_issue(journal, vol, iss):
	url = base_url.format(journal, vol, iss)
	hdr = {'User-Agent': choice(user_agents)}
	req = requests.get(url, headers=hdr)
	links = []
	abstrs = []
	soup = BeautifulSoup(req.content, parse_only=SoupStrainer('a'))
	for link in soup('a'):
		if link.has_key('href'):
			if is_fulltext(to_ascii(link.text)):
				links.append(link)
			if is_abstract(to_ascii(link.text)):
				abstrs.append(link)
	
	links = map(lambda x: 'http://www.nature.com' + x['href'], links)
	abstrs = map(lambda x: 'http://www.nature.com' + x['href'], abstrs)

	files = links if len(links) >= len(abstrs) else abstrs
	for link in files:
		process_article(link, journal)

# only run once; DO NOT RUN multiple times, and only run BEFORE other crawling!
def initialize_last_crawled():
	l_c = {}
	for j in journals_kindof:
		if j == 'cti':
			l_c[j] = ('1', '10')
		else:
			fa = first_available(j)
			l_c[j] = (str(fa - 1), '0')
			print "{0}, {1}".format(j, str(fa))
		update_last_crawled(l_c)

# should be in volume, issue format
def load_last_crawled():
	with open(last_file, 'r') as f:
		mtch = re.findall(format_regex, f.read())
		dct = dict(map(lambda x: (x[0], (int(x[1]), int(x[2]))), mtch))
	return dct

# run after crawling
def update_last_crawled(dct):
	with open(last_file, 'w') as f:
		for k, v in dct.items():
			f.write(k + ': ' + v[0] + ' ' + v[1] + '\n')

# returns the modified dictionary
def crawl_since_last_and_update(journal, last_crawled):
	vol, iss = last_available(last_crawled[journal][0])
	if vol == last_crawled[journal][0] and iss ==  last_crawled[journal][1]:
		print "Nothing to do for journal {0}.".format(journal)
		return
	else:
		for i in range(last_crawled[journal][1], vol + 1):
			j = iss + 1 if i == last_crawled[journal][1] else 1
			url = base_url.format(journal, str(i), str(j))
			hdr = {'User-Agent': choice(user_agents)}
			page = requests.get(url, header=hdr).text
			while issue_exists(page):
				get_issue(journal, i, j)
				j += 1
				url = base_url.format(journal, str(i), str(j))
				hdr = {'User-Agent': choice(user_agents)}
				page = requests.get(url, hdr).text
	last_crawled[journal][0] = vol
	last_crawled[journal][1] = iss

# for simplicity, only run this
def update():
	last_crawled = load_last_crawled()
	for j in journals:
		crawl_since_last_and_update(j, last_crawled)
	crawl_since_last_and_update_nature()
	update_last_crawled(last_crawled)
	update_last_crawled_nature(datetime.date.today())

# everything here works for nature only
date_regex = re.compile('(\d{2,2}) \
	(January|February|March|April|\
	May|June|July|August|September|\
	October|November|December) (\d{4,4})')
nature_first_year = 1950
nature_archive = 'http://www.nature.com/nature/archive/index.html'
last_nature_file = base_path + 'Crawling/Nature/last_nature.txt'
months = {'January':1, 'February':2, 'March':3, 'April':4,
	'May':5, 'June':6, 'July':7, 'August':8,
	'September':9, 'October':10, 'November':11, 'December':12}

def to_datetime(dt):
	return  datetime.date(int(dt[2]), months[dt[1]], int(dt[0]))

def last_available_nature():
	hdr = {'User-Agent': choice(user_agents)}
	req = requests.get(nature_archive, headers=hdr)
	soup = BeautifulSoup(req.content, parse_only=SoupStrainer('a'))
	dt = str(soup.find(text=date_regex)).split(' ')
	return to_datetime(dt)

def last_crawled_nature():
	with open(last_nature_file, 'r') as f:
		return datetime.date(*tuple(map(int, f.read().split('-'))))

def update_last_crawled_nature(dt):
	with open(last_nature_file, 'w') as f:
		f.write(str(dt))

def get_issue_nature(url):
	hdr = {'User-Agent': choice(user_agents)}
	req = requests.get(url, headers=hdr)
	soup = BeautifulSoup(req.content, parse_only=SoupStrainer('a'))
	links = soup('href', lambda x: 'nature' in x['href'].split('/')[-1])
	links = []
	for link in soup('a'):
		if link.has_key('href'):
			if 'nature' in link['href'].split('/')[-1]:
				links.append(link)
	for link in links:
		process_article(link, journal)

# crawling everything since 1980
def crawl_since_last_and_update_nature():
	old = last_crawled_nature()
	now = datetime.date.today()
	url = nature_archive + '?year=%s' % now.year
	for year in range(old.year, now.year):
		url = url + '-' + str(year)
	hdr = {'User-Agent': choice(user_agents)}
	req = requests.get(url, headers=hdr)
	soup = BeautifulSoup(req.content, parse_only=SoupStrainer('a'))
	links = []
	for link in soup('a'):
		dt = re.search(date_regex, link.text)
		if dt is not None:
			if to_datetime(str(dt.group(0)).split(' ')) > old:
				links.append(link)
	links = map(lambda x: 'http://www.nature.com' + x['href'] if \
		'www' not in x['href'] else x['href'], links)
	links = filter(lambda x: 's' not in x, links)
	for link in link:
		get_issue_nature(url)
