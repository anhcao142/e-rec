import lib

print 'Reformatting User-Event data'

r = lib.reformatUserEvent('../../event-user.csv', '../../user-event.csv')

if r == 1:
    print 'DONE'