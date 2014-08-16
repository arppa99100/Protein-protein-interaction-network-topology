def slice_timepoints(proportions, values, step=0.001):
  """
  This is a generator
  proportions: x-axis values, proportion of estimated interactome size
  values: y-axis values, dependent on measure used
  """
  (x0,y0)=(None,None)
  for (x1,y1) in zip(proportions,values):
    if(x0==None):
      (x0,y0)=(x1,y1)
      start=x0
      continue
    slope=(y1-y0)/float((x1-x0));
    b=y0-slope*x0;
    while(start <= x1):
      yield (start,slope*start+b)
      start+=step
    # print("start=",start)
    (x0,y0)=(x1,y1)


def cumulative_sma(bar, series, prevma):
  """
  Returns the cumulative or unweighted simple moving average.
  Avoids sum of series per call.

  Keyword arguments:
  bar     --  current index or location of the value in the series
  series  --  list or tuple of data to average
  prevma  --  previous average (n - 1) of the series.
  """

  if bar <= 0:
    return series[0]

  else:
    return prevma + ((series[bar] - prevma) / (bar + 1.0))

def running_sma(bar, series, period, prevma):
  """
  Returns the running simple moving average - avoids sum of series per call.

  Keyword arguments:
  bar     --  current index or location of the value in the series
  series  --  list or tuple of data to average
  period  --  number of values to include in average
  prevma  --  previous simple moving average (n - 1) of the series
  """
  if period < 1:
    raise ValueError("period must be 1 or greater")

  if bar <= 0:
    return series[0]

  elif bar < period:
    return cumulative_sma(bar, series, prevma)

  return prevma + ((series[bar] - series[bar - period]) / float(period))

def powersumavg(bar, series, period, pval=None):
  """
    Returns the power sum average based on the blog post from
    Subliminal Messages.  Use the power sum average to help derive the running
    variance.
    sources: http://subluminal.wordpress.com/2008/07/31/running-standard-deviations/

    Keyword arguments:
    bar     --  current index or location of the value in the series
    series  --  list or tuple of data to average
    period  -- number of values to include in average
    pval    --  previous powersumavg (n - 1) of the series.
  """
  if period < 1:
    raise ValueError("period must be 1 or greater")

  if bar < 0:
    bar = 0

  if pval == None:
    if bar > 0:
      raise ValueError("pval of None invalid when bar > 0")

    pval = 0.0

  newamt = float(series[bar])

  if bar < period:
    result = pval + (newamt * newamt - pval) / (bar + 1.0)

  else:
    oldamt = float(series[bar - period])
    result = pval + (((newamt * newamt) - (oldamt * oldamt)) / period)

  return result

def running_var(bar, series, period, asma, apowsumavg):
  """
    Returns the running variance based on a given time period.
    sources: http://subluminal.wordpress.com/2008/07/31/running-standard-deviations/

    Keyword arguments:
    bar     --  current index or location of the value in the series
    series  --  list or tuple of data to average
    asma    --  current average of the given period
    apowsumavg -- current powersumavg of the given period
    """
  if period < 1:
    raise ValueError("period must be 1 or greater")

  if bar <= 0:
    return 0.0

  if asma == None:
    raise ValueError("asma of None invalid when bar > 0")

  if apowsumavg == None:
    raise ValueError("powsumavg of None invalid when bar > 0")

  windowsize = bar + 1.0
  if windowsize >= period:
    windowsize = period

  return (apowsumavg * windowsize - windowsize * asma * asma) / windowsize

####################
####################
####################
def run(xaxis, yaxis):
    list_of_xaxis=list()
    list_of_yaxis=list()
    final_x, final_y = [], []
    for (newx,newy) in slice_timepoints(xaxis, yaxis, step=(xaxis[-1] - xaxis[0])/100):
      list_of_xaxis.append(newx)
      list_of_yaxis.append(newy)

    # print(list_of_xaxis[0],list_of_xaxis[-1])
    # exit()
    prev_powersumavg = None
    prev_sma = None
    prev_sma = None
    period = 10
    for bar, price in enumerate(list_of_yaxis):
      new_sma = running_sma(bar, list_of_yaxis, period, prev_sma)
      new_powersumavg = powersumavg(bar,
          list_of_yaxis,
          period,
          prev_powersumavg)
      new_var = running_var(bar,
          list_of_yaxis,
          period,
          new_sma,
          new_powersumavg)

      #msg = "SMA=%.4f, PSA=%.4f, Var=%.4f" % (new_sma, new_powersumavg, new_var)
      msg = "Var=%.4f" % (new_var)
      # print "bar %.4f: %s" % (list_of_xaxis[bar], msg)

      final_x.append(list_of_xaxis[bar])
      final_y.append(new_var)


      prev_sma = new_sma
      prev_powersumavg = new_powersumavg
    # print(final_x[0], xaxis[0])
    # print(final_x[-1], xaxis[-a1])
    # print(final_x,final_y)
    # input()
    return final_x, final_y

yaxis = [3,    5,   8, 10,  4,  8,  12, 15, 11, 9]
xaxis = [.01, .02, .09, .2, .3, .35, .5, .7, .75, .8]
run(xaxis, yaxis)
