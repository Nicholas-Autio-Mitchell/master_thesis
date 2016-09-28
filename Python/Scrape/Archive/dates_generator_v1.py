import itertools
from datetime import date, timedelta

# if __name__ == "__main__":
#     # Define date range - we are moving backwards into time, therefore we begin in 2015
#     start_year = 2015
#     start_month = 9
#     start_day = 15
#     end_year = 2013
#     end_month = 1
#     end_day = 1

#    # Define how many days should constitute one block i.e. the difference between dates in teh generated list
#    granularity = 15

    
def dates_generator(start_year, start_month, start_day, end_year, end_month, end_day, granularity):

    # Define a generator function to produce a date list
    def series_of_dates(start, end, delta):
        current = start
        while current > end:
            yield current
            current -= delta

    # Define a class that we use as a wrapper to make our generator object indexable
    class Indexable(object):
        def __init__(self,it):
            self.it = iter(it)
        def __iter__(self):
            for elt in self.it:
                yield elt
        def __getitem__(self,index):
            try:
                return next(itertools.islice(self.it,index,index+1))
            except TypeError:
                return list(itertools.islice(self.it,index.start,index.stop,index.step))
 
    start = date(start_year, start_month, start_day)
    end = date(end_year, end_month, end_day)
    intervals = timedelta(days=granularity)

    # Create an indexable list of strings from the generated dates
    generated_dates = series_of_dates(start, end, intervals)
    indexable_dates = Indexable(generated_dates)
    dates = [x.strftime("%Y-%m-%d") for x in indexable_dates]

    # # Append the last date to get a complete (in this case for static input above)
    # dates.append("2012-12-31")
    
    return dates

if __name__ == "__main__":

    # Define date range - we are moving backwards into time, therefore we begin in 2015
    start_year = 2015
    start_month = 9
    start_day = 15
    end_year = 2013
    end_month = 1
    end_day = 1

    # Define how many days should constitute one block i.e. the difference between dates in teh generated list
    granularity = 15

    myDates = dates_generator(start_year, start_month, start_day, end_year, end_month, end_day, granularity)
