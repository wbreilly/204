# PSC - 204b Final
# Walter Reilly
# Last update: 12/3/16
#
#

 ########################################
# #2
# using algrebra (attached hand calculations), filled in covariance table. Now will use matrix multiplication
# to fill in remainder of correlation table and check work

 # first create matrix of 1/SDs with SDs on diagonal 
 tocorr.mtx = matrix( rep( 0, len=25), nrow = 5)
 tocorr.mtx[1,1] = 1/sqrt(4)
 tocorr.mtx[2,2] = 1/sqrt(.25)
 tocorr.mtx[3,3] = 1/sqrt(9)
 tocorr.mtx[4,4] = 1/sqrt(7.29)
 tocorr.mtx[5,5] = 1/sqrt(29.16)
 
 # imported csv of the full covariance matrix, which was copied from the Word doc.
 # I used Excel to copy the matrix across the diagonal
corrmx = tocorr.mtx %*% covmtx %*% tocorr.mtx\
corrmx
 
# double check covariance matrix by reproducing from corr matrix
sd.mtx = matrix( rep( 0, len=25), nrow = 5)
sd.mtx[1,1] = sqrt(4)
sd.mtx[2,2] = sqrt(.25)
sd.mtx[3,3] = sqrt(9)
sd.mtx[4,4] = sqrt(7.29)
sd.mtx[5,5] = sqrt(29.16)

covarmx = sd.mtx %*% corrmx %*% sd.mtx
covarmx


