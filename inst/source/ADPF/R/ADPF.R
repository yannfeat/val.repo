#' @title Adaptive Degree Polynomial Filter [ADPF]
#' @description
#' ADPF outputs a \code{data.frame} containing a column for the original data, the polynomial degree used to smooth it, and the requested derivative(s).
#' @usage ADPF(YData, SthDeriv,MaxOrder,FilterLength, DeltaX, WriteFile)
#' @param YData a numeric \code{data.frame}, \code{matrix} or \code{vector} to transform
#' @param SthDeriv differentiation order
#' @param MaxOrder maximum polynomial order
#' @param FilterLength window size (must be odd)
#' @param DeltaX optional sampling interval
#' @param WriteFile a boolean that writes a \code{data.frame} to the working directory if true
#' @author Phillip Barak
#' @author Samuel Kruse
#' @export
#' @importFrom stats qf
#' @importFrom utils write.csv
#' @examples
#'
#' ADPF::CHROM
#'
#' smooth<-ADPF(CHROM[,6],0,9,13)
#' numpoints=length(CHROM[,6])
#' plot(x=1:numpoints,y=CHROM[,6]);lines(x=1:numpoints, y=smooth[,3])
#' @details This is a code listing of a smoothing algorithm published in 1995 and written by Phillip Barak. ADPF modifies the Savitzky-Golay algorithm with a statistical heurism that increases signal fidelty while decreasing statisical noise.
#' Mathematically, it operates simply as a weighted sum over a given window:
#' \deqn{f_{t}^{n,s}=\sum{_{i=-m}^{m} h_{i}^{n,s,t}y_{i}}}
#' Where \eqn{h_{i}^{n,s,t}} is the convolution weight of the \eqn{i}th point to the evaluate the \eqn{s}th derivative at point \eqn{t} using a polynomial of degree \eqn{n}
#' on 2\eqn{m+1} data points, \eqn{y}. These convolution weights \eqn{h} are calculated using Gram polynomials which are optimally selected using a \eqn{F_{chi}} test.
#' This improves upon the signal fidelity of Savitzky-Golay by optimally choosing the Gram polynomial degree between zero and the max polynomial order give by the user while removing statistical noise.
#' The sampling interval specified with the \code{DeltaX} argument is used for scaling and get numerically correct derivatives. For more details on the statistical heurism see the Barak, 1995 article. This can be found at http://soils.wisc.edu/facstaff/barak/ under the publications section.
#' @references Barak, P., 1995. Smoothing and Differentiation by and Adaptive-Degree Polynomial filter; Anal. Chem. 67, 2758-2762.
#' 
#' Marchand, P.; Marmet, L. Rev. Sci. Instrum. 1983, 54, 1034-1041.
#' 
#' Greville, T. N. E., Ed. Theory and Applications of Spline Functions; Academic Press: New York, 1969.
#' 
#' Press, W. H.; Flannery, B. P.; Teukolsky, S. A.;Vetterling. W. T. Numerical Recipes; Cambridge University Press: Cambridge U.K., 1986.
#'
#' Savitzky, A., and Golay, M. J. E., 1964. Smoothing and differentiation of data by simplified least squares procedures. Anal. Chem. 36, 1627-1639.
#' 
#' Macauly, F. R. The Smoothing of Time Series; National Bureau of Economic Research, Inc,: New York, 1931.
#' 
#' Gorry, P. A. Anal. Chem. 1964, 36,1627-1639.
#' 
#' Steiner, J.; Termonia, Y.; Deltour, J. Anal. Chem. 1972, 44. 1906-1909.
#' 
#' Ernst, R. R. Adv. magn. Reson. 1966, 2,1-135.
#' 
#' Gorry P. A. Anal. Chem. 1991, 64, 534-536.
#' 
#' Ratzlaff, K. L.; Johnson, J. T. Adal. Chem. 1989, 61, 1303-1305.
#' 
#' Kuo, J. E.; Wang, H.; Pickup, S. Anal. Chem. 1991, 63,630-645.
#' 
#' Enke, C. G; Nieman, T. A. Anal. Chm 1976, 48, 705A-712A.
#' 
#' Phillips, G. R., Harris, J. M. Anal. Chem. 1990, 62, 2749-2752.
#' 
#' Duran, B.S. Polynomial Regression. In Encyclopedia of the Statistical Sciences, Kotz, S., Johnsonn N. L., Eds.; Wiley: New York, 1986; Vol. 7, pp 700-703.
#' 
#' Bevington, P. R. Data Reduction and Error Analysis for the Physical Sciences; McGraw-Hill Book Co,: New York, 1969; Chapter 10.
#' 
#' Snedecor, G. W.; Cochran, W. G. Statistical Methods, 6th ed.; Iowa State University Press: Ames, IA, 1967; Chapter 15.
#' 
#' Hanning, R. W. Digital Filters, 2nd ed.; Prentice-Hall: Englewood Cliffs, NJ, 1983; Chapter 3.
#' 
#' Ralston, A. A First Course in Numerical Analysis McGraw-Hill: New York, 1965; Chapter 6.
#' 
#' Robert De Levie. 2008. Advanced Excel for Scientific data analysis. 2nd edn. Chapter 3.15 Least squares for equidistant data. Oxford Univ. Press, New York, NY.
#' 
#' Wentzell, P. D., and Brown, C. D., 2000. Signal processing in analytical chemistry. Encyclopedia of Analytical Chemistry, 9764-9800.


ADPF <-
  function(YData,
           SthDeriv,
           MaxOrder,
           FilterLength,
           DeltaX,
           WriteFile) {
    if (missing(DeltaX))
      DeltaX <- 1
    if (missing(WriteFile))
      WriteFile = FALSE
    if (is.data.frame(YData))
      YData <- as.matrix(YData)
    
    if (FilterLength %% 2 != 1)
      stop("needs an odd filter length")
    if (MaxOrder >= FilterLength)
      stop("filter length should be greater than maximum polynomial order")
    if (MaxOrder < SthDeriv)
      stop("polynomial order should be geater or equal to differentiation order")
    if (SthDeriv > 2)
      stop("Derivative should be less than or equal to 2")
    #this is the function that ultimately combines the convolution weights with the original data points
    
    
    
    
    NPts <- length(YData)
    MinOrder <- 0
    
    
    sgbsmooth <- array(dim = c(NPts, 5))
    sgbsmooth[, 1] <- YData
    colnames(sgbsmooth) = c("YData",
                            "Jsignif",
                            "smooth",
                            "1st Derivative",
                            "2nd Derivative")
    
    
    
    GenFact <- function(A1, B1) {
      gf = 1
      C1 = A1 - B1 + 1
      if (C1 > A1) {
        return(gf)
      } else{
        for (j in C1:A1) {
          gf = gf * j
        }
      }
      return(gf)
      
    }
    
    m <- (FilterLength - 1) / 2
    
    q <- FilterLength - m
    
    
    GramPoly = array(dim = c(FilterLength, MaxOrder + 2, SthDeriv + 2))
    for (i in 1:FilterLength) {
      GramPoly[i, 1, 2] = 0
      GramPoly[i, 2, 2] = 1
    }
    for (j in-m:m) {
      n = j + q
      GramPoly[n, 3, 2] = j / m
    }
    for (k in 2:MaxOrder) {
      A2 = 2 * (2 * k - 1) / (k * (2 * m - k + 1))
      B2 = ((k - 1) * (2 * m + k)) / (k * (2 * m - k + 1))
      
      for (i in 0:m) {
        GramPoly[i + q, k + 2, 2] = A2 * i * GramPoly[i + q, k + 1, 2] - B2 * GramPoly[i +
                                                                                         q, k, 2]
        
        
      }
      
      for (i in 1:m) {
        if (k %% 2 == 0) {
          GramPoly[i, k + 2, 2] <- GramPoly[FilterLength + 1 - i, k + 2, 2]
        } else{
          GramPoly[i, k + 2, 2] <-
            -1 * (GramPoly[FilterLength + 1 - i, k + 2, 2])
        }
        
        
      }
    }
    
    
    if (SthDeriv > 0) {
      for (s in 1:SthDeriv) {
        for (i in 1:FilterLength) {
          GramPoly[i, 1, s + 2] = 0
        }
        for (i in 1:FilterLength) {
          GramPoly[i, 2, s + 2] = 0
        }
        for (k in 1:MaxOrder) {
          A1 = 2 * (2 * k - 1) / (k * (2 * m - k + 1))
          B1 = ((k - 1) * (2 * m + k)) / (k * (2 * m - k + 1))
          for (i in-m:m) {
            GramPoly[i + q, k + 2, s + 2] = A1 * (i * GramPoly[i + q, k + 1, s + 2] + s *
                                                    GramPoly[i + q, k + 1, s + 1]) - B1 * GramPoly[i + q, k, s + 2]
            
          }
        }
      }
    }
    
    
    
    
    #This is the array of weights that uses gram-polynomials and recursion to produce desired values
    Weight <-
      array(dim = c(FilterLength, FilterLength, MaxOrder + 2, SthDeriv + 2))
    
    for (k in 0:MaxOrder) {
      A = (2 * k + 1) * GenFact(2 * m, k) / GenFact(2 * m + k + 1, k + 1)
      
      for (s in 0:SthDeriv) {
        for (i in 1:FilterLength) {
          for (t in 1:FilterLength) {
            Weight[i, t, 1, s + 2] <- 0
            Weight[i, t, k + 2, s + 2] = Weight[i, t, k + 1, s + 2] + A * GramPoly[i, k +
                                                                                     2, 2] * GramPoly[t, k + 2, s + 2]
            
          }
        }
      }
    }
    
    
    
    
    
    SumX2 <- c(1:MaxOrder)
    
    for (j in 1:MaxOrder) {
      Sum = 0
      for (i in 1:FilterLength) {
        Sum <- Sum + (GramPoly[i, j + 2, 2]) ^ 2
      }
      SumX2[j] <- Sum
      
    }
    
    
    FValueTable <- array(dim = c(MaxOrder, FilterLength))
    
    dF2 = 0
    while (dF2 < FilterLength) {
      dF2 = dF2 + 1
      
      for (i in 1:MaxOrder) {
        FValueTable[i, dF2] <- qf(0.05, i, dF2, lower.tail = FALSE)
      }
      
    }
    
    
    
    
    
    Y <- c(FilterLength)
    SumSquares <- c(MaxOrder + 1)
    Ftest <- c(MaxOrder + 1)
    
    JSignif = 0 #set at 0 for first fit;all others calc off previous fit
    
    SGSmooth <- function(j, t, Y, s) {
      Sumsg = 0
      for (i in 1:FilterLength) {
        Sumsg = Sumsg + Weight[i, t, j + 2, s] * Y[i]
        
      }
      
      return(Sumsg)
    }
    
    for (k in q:(NPts - m)) {
      for (i in-m:m) {
        Y[i + q] <- YData[k + i]
      }
      #rezero arrays
      for (j in 1:MaxOrder + 1) {
        SumSquares[j] = 0
        Ftest[j] = 0
      }
      #Calc Sum of squares for start point
      
      if (JSignif == 0) {
        SumY = 0
        SumY2 = 0
        for (i in 1:FilterLength) {
          SumY = SumY + Y[i]
          SumY2 = SumY2 + (Y[i]) ^ 2
        }
        SumSquares[1] = SumY2 - ((SumY) ^ 2) / (2 * m + 1)
        
      } else{
        SumSq = 0
        for (t in 1:FilterLength) {
          SumSq = SumSq + (SGSmooth(JSignif, t, Y, 2) - Y[t]) ^ 2
        }
        SumSquares[JSignif + 1] = SumSq
      }
      
      j = JSignif + 1
      
      repeat {
        SumXY = 0
        for (p in 1:FilterLength) {
          SumXY = SumXY + Y[p] * GramPoly[p, j + 2 , 2]
        }
        SumSquares[j + 1] = SumSquares[j] - SumXY ^ 2 / SumX2[j]
        
        #calc F-test againsts last significant order Jsignif
        
        Ftest[j + 1] = (SumSquares[JSignif + 1] - SumSquares[j + 1]) / (SumSquares[j +
                                                                                     1] / ((2 * m + 1) - j - 1))
        
        
        
        dF1 = (j - JSignif)
        
        dF2 = (FilterLength - j - 1)
        
        FValue = FValueTable[dF1, dF2]
        
        if (Ftest[j + 1] > FValue) {
          JSignif = j
          
        }
        j = j + 1
       
        if ((JSignif + 2) >= MinOrder) {
          MinOrder = JSignif + 2
          if (MinOrder > MaxOrder) {
            MinOrder = MaxOrder
            
          }
        }
        
        if (j > MinOrder) {
        
          break
        }
        
      }
      
      if (JSignif == 0 || JSignif == 1) {
        MinMax = 0
      } else{
        if (JSignif == 2) {
          MinMax = 1
        } else{
          MinMax = 0
          OldestY = SGSmooth(JSignif, 1, Y, 2)
          OldY = SGSmooth(JSignif, 2, Y, 2)
          OldSign = sign(OldY - OldestY)
          for (t in 3:FilterLength){
            NewY = SGSmooth(JSignif, t, Y, 2)
          Sign = sign(NewY - OldY)
          if (Sign != OldSign) {
            MinMax = MinMax + 1
          }
          OldSign = Sign
          OldY = NewY
        }
      }
      }
     
      #this if statement fills out first m spots with the qth polynomial
      if (k == q) {
        for (t in 1:m) {
          sgbsmooth[t, 3] <- SGSmooth(JSignif, t, Y, 2)
          if (SthDeriv > 0) {
            sgbsmooth[t, 4] = SGSmooth(JSignif, t, Y, 3) / DeltaX
          }
          if (SthDeriv == 2) {
            sgbsmooth[t, 5] = SGSmooth(JSignif, t, Y, 4) / DeltaX ^ 2
          }
        }
      }
      
      sgbsmooth[k, 3] <- SGSmooth(JSignif, q, Y, 2)
      
      
      sgbsmooth[k, 2] <- JSignif
      
      if (SthDeriv > 0) {
        sgbsmooth[k, 4] = SGSmooth(JSignif, q, Y, 3) / DeltaX
        
      }
      if (SthDeriv == 2) {
        sgbsmooth[k, 5] = SGSmooth(JSignif, q, Y, 4) / DeltaX ^ 2
      }
      if (k == (NPts - m)) {
        for (t in 1:m) {
          sgbsmooth[k + t, 3] = SGSmooth(JSignif, t + q, Y, 2)
          if (SthDeriv > 0) {
            sgbsmooth[k + t, 4] = SGSmooth(JSignif, t + q, Y, 3) / DeltaX
          }
          if (SthDeriv == 2) {
            sgbsmooth[k + t, 5] = SGSmooth(JSignif, t + q, Y, 4) / DeltaX ^ 2
          }
        }
        
      }
      MinOrder = MinMax + 1
      
      if (MinOrder > MaxOrder) {
        MinOrder = MaxOrder
        
      }
      if (MinOrder < 0) {
        MinOrder = 0
      }
      JSignif = 0
    }
    
    #print("Smoothed Values are saved in Files Tab and printed below!")
    if (WriteFile == TRUE) {
      write.csv(sgbsmooth, "smooth")
    }
    if (SthDeriv == 0) {
      return(sgbsmooth[, 1:3])
    }
    if (SthDeriv == 1) {
      return(sgbsmooth[, 1:4])
    }
    if (SthDeriv == 2) {
      return(sgbsmooth[, 1:5])
    }
    
  }
