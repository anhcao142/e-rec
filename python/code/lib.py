import csv
from operator import itemgetter
from sys import stdout
from time import sleep
from dateutil import parser
import progressbar
import datetime

bar = progressbar.ProgressBar(max_value=progressbar.UnknownLength)

def reformatUserEvent(originalFileName, newFileName):
    with open(originalFileName, 'rb') as csvfile:
        writeFile = open(newFileName, 'wb')
        writer = csv.writer(writeFile, delimiter = ',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
        reader = csv.reader(csvfile, delimiter = ',')

        writer.writerow(['userID', 'eventID', 'status'])

        next(reader)
        for row in reader:
            newRow = ['', row[0]]
            if row[1] != '':
                newRow[0] = row[1]
                newRow.append(1)
            elif row[2] != '':
                newRow[0] = row[2]
                newRow.append(-1)
            else:
                newRow[0] = row[3]
                newRow.append(0)

            writer.writerow(newRow)
        writeFile.close()
        return 1

def removeUserEvent(filename, eventCountRequire, attendLimit):
    with open(filename, 'rb+') as csvfile:
        data = []
        reader = csv.reader(csvfile)
        header = next(reader)

        for row in reader:
            data.append(row)

        #Sort data by userID
        data = sorted(data, key = itemgetter(0))
        oldLength = len(data)

        preUserID = -1
        existCount = 0      #Number of events an user interact with (attend, declined or maybe)
        attendCount = 0     #Number of events an user attend
        i = 0               #Index
        l = len(data)       #Data length
        while i < l - 1:
            curUserID = data[i][0]
            status = data[i][2]

            if preUserID == curUserID:
                existCount += 1
            else:
                preUserID = curUserID
                if (existCount < eventCountRequire and existCount > 0) or attendCount > attendLimit:
                    if existCount == 1:
                        data.pop(i - 1)
                    else:
                        for k in reversed(range(i - existCount, i)):
                            data.pop(k)
                    i -= existCount
                    l -= existCount

                existCount = 1
                attendCount = 0

            if status == 1:     # 1: attend 0: maybe -1: declined
                attendCount += 1

            i += 1

        #Clear file to write new data
        csvfile.seek(0)
        csvfile.truncate()

        #Write new data to file
        writer = csv.writer(csvfile)
        writer.writerow(header)
        writer.writerows(data)

        print 'Remove %d rows' % (oldLength - len(data))
        return 1

def reprocessEvent(filename, outFileName):
    with open(filename, 'rb') as csvfile:
        reader = csv.reader(csvfile)

        writeCsvFile = open(outFileName, 'wb')
        writer = csv.writer(writeCsvFile)

        writer.writerow(['eventId', 'start_time', 'during_time', 'latitude', 'longitude'])

        #skip first row
        next(reader)
        for row in reader:
            newRow = [row[0], '', '', '', '']

            #Only accept long lat if both data are provided
            if row[3] != '' and row[4] != '':
                newRow[3] = row[3]
                newRow[4] = row[4]


            if row[1] != '':
                start = parser.parse(row[1]).replace(tzinfo=None)
                dow = start.weekday()

                #start.weekday() == 0 (Monday). We start new week at sunday
                #so we have to do some job to make that right
                if dow == 6:
                    newRow[1] = start.hour
                else:
                    newRow[1] = 24 * (1 + dow) + start.hour

                #Only accept enddate if startdate is provided
                if row[2] != '':
                    end = parser.parse(row[2]).replace(tzinfo=None)
                    timediff = end - start
                    newRow[2] = timediff.days * 24 + timediff.seconds/3600.0
                elif row[1] != '':
                    #If endDate date is missing, using 3 as default value
                    newRow[2] = 3.0

            writer.writerow(newRow)
        return 1

#reprocessEvent('../../events.csv', './events.csv')

# reformatUserEvent('../../event-user.csv', '../../user-event.csv')
# removeUserEvent('../../user-event.csv', 2)
