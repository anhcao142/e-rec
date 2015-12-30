try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

config = {
    'description': 'Reprocess data for event recommendation',
    'author': 'boylove142',
    'url': '',
    'download_url': '',
    'author_email': 'caodunganh@gmail.com',
    'version': '0.1',
    'install_requires': ['nose', 'itemgetter'],
    'packages': ['code'],
    'scripts': [],
    'name': 'projectname'
}

setup(**config)