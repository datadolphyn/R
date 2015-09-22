# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

#setContentType ("image/png")
temp <- tempfile ()
y = rnorm (100)
png (temp, type="cairo")
plot (1:100, y, t='l')
dev.off ()
sendBin (readBin (temp, 'raw', n=file.info(temp)$size))
unlink (temp)