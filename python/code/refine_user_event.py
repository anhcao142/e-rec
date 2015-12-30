import lib

print 'Start refining User-Event data'

r = lib.removeUserEvent('../../user-event.csv', 2, 100)

if r == 1:
    print 'DONE'