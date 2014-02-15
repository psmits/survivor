import webbrowser
import pandas

pages = pandas.read_csv('../data/pap_tit.csv', header = 1)

print pages.index

#webbrowser.open(url='http://en.wikipedia.org/wiki/Main_Page',new=2)
