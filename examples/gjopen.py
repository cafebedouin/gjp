# Given lowPoint < highPoint, we want a density function that grows linearly
# from (lowPoint, 0) to (someMidPoint, h), then decreases linearly from
# (someMidPoint, h) to (highPoint, 0). This method finds h to make integral 1.
def GetHeightOfMode(lowPoint, highPoint):
..return 2.0/(highPoint - lowPoint)

def DensityFunction(lowPoint, midPoint, highPoint, x):
..h = GetHeightOfMode(lowPoint, highPoint)
..risingSlope = h/(midPoint - lowPoint)
..fallingSlope = -h/(highPoint - midPoint)
..if x <= lowPoint:
....return 0.0
..if lowPoint < x <= midPoint:
....return (x - lowPoint)*risingSlope
..if midPoint < x <= highPoint:
....return h + (x - midPoint)*fallingSlope
..if highPoint < x:
....return 0.0

# Returns a density function that grows linearly from (lowPoint, 0) to
# (someMidPoint, h), then decreases linearly from (someMidPoint, h) to
# (highPoint, 0).
def GetDensityFunction(lowPoint, midPoint, highPoint):
..return (lambda x : DensityFunction(lowPoint, midPoint, highPoint, x))

# Density functions for US & China production
[usLow, usMid, usHigh] = [260, 302, 380]
[chinaLow, chinaMid, chinaHigh] = [0, 1, 5]
US = GetDensityFunction(usLow, usMid, usHigh)
China = GetDensityFunction(chinaLow, chinaMid, chinaHigh)

totalProduction = [0]*(usHigh+chinaHigh+1)
for s in range(usLow+chinaLow,usHigh+chinaHigh+1):
..totalProduction[s] = sum([ US(i)*China(s-i) for i in range(0,s+1) ])

buckets = [0]*5
buckets[0] = sum(totalProduction[0:281])
buckets[1] = sum(totalProduction[281:331])
buckets[2] = sum(totalProduction[331:381])
buckets[3] = sum(totalProduction[381:431])
buckets[4] = sum(totalProduction[431:])
