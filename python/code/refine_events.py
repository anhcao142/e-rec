import lib

print 'Start refining Events data'

r = lib.reprocessEvent('../../events.csv', './events.csv')

if r == 1:
    print 'DONE'