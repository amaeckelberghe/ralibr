#' Interpolation
#'
#' @description Function useful for Interpolation.
#'    can be either "Dates" or "Numeric", allowing to
#'    easily Interpolate on discount curves.
#'    The interpolation can be done using linear or cubic-spline.
#'
#' @param X,Y Vectors giving the coordinates of the points to be interpolated.
#'    X can optionally be "Date" format,
#'    allowing to interpolate on a typical j
#'    discount curve structure.
#'
#' @param x A single point specifying where to interpolate on observations of X.
#'    Can be numeric, Date or character ("YYYY-MM-DD")
#'
#' @param method Either linear ("l") or cubicspline ("cs")
#'
#' @return The value corresponding to x of values in Y.
#'
#' @examples
#' Interpolate(X = c(1,3,5),Y = c(100,200,300),x = 2,method="l")
#' Interpolate(Curve$Date, Curve$Discount_Factors, "2025-03-03", "linear")
#'
#' @export

interpolate <- function(X, Y, x, method = "linear"){

        # Convert the Vector X values in Column 1 to integers
        if(class(x)=="character"){
                x <- ParseDate(DateToParse = x)
        }else if (class(X)=="Date"){
                X <- as.numeric(X)
        }


        # Check if Point is inside the Vector X range
        if (any(x<min(X))) stop("Point < min(X)")

        # Interpolate given method
        if (method=="linear"|method=="lin"|method=="l"){
                if(x>max(X)){stop("x > max(X) - Use method = 'cs' for Extrapolation")}
                Interpolate <- approx(x = X,y = Y,xout = x)[[2]]
        }
        else if (method=="cs"|method=="cubicspline"|method=="spline"){
                if(x>max(X)){warning("x > max(X) - Extrapolating")}
                Interpolate <- spline(x = X,y = Y,method = "natural",xout = x)[[2]]
        }
        else stop("Input 'method' is wrong")

        return(Interpolate)
}
