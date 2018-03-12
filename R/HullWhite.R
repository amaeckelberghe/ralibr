#' Calibration of one-factor Hull-White parameters from Swaption volatilies
#'
#' @param valdate
#' @param dsc_cv
#' @param fwd_cv
#' @param vols
#'
#' @return
#' @export
#'
#' @examples
calibrate_hull_white_swaption <-
        function(valdate, cvDiscount, cvTenor, vols) {
                # Convert the vols
                swaption_data <-
                        convert_swaption_vols(
                                vols = vols,
                                cvTenor = cvTenor,
                                cvDiscount = cvDiscount,
                                valdate = valdate
                        )

                return(swaption_data)

        }

#' Title
#'
#' @param SwaptionVols
#' @param TenorCurve
#' @param DiscountCurve
#' @param ValDate
#'
#' @return
#'
#' @importFrom bizdays offset
#' @importFrom bizdays create.calendar
#'
#' @examples
convert_swaption_vols <-
        function(vols,
                cvTenor,
                cvDiscount,
                valdate) {

                # Dimensions of the Swaption Vol Cube
                N <- dim(vols)[1]
                M <- dim(vols)[2]

                # Pre-allocate matrices
                ATMStrikes <- matrix(nrow = N, ncol = M)
                Annuity <- matrix(nrow = N, ncol = M)
                Prices <- matrix(nrow = N, ncol = M)

                # Swaption Tenor from the input (!)
                Tenor <-
                        substr(x = names(vols)[1],
                                start = 4,
                                stop = 4)

                # Convert Tenor to a string
                TenorString <- tenor_to_string(tenor = Tenor)

                # Pre-allocate list of Date combinations (Expiry-Tenors)
                Dates = list()

                # Parse valdate
                valdate <-
                        parse_date_internal(DateToParse = valdate, DateType = "European")

                # Create a calendar
                create.calendar("Actual", weekdays = c("saturday", "sunday"))

                # We loop over the rows and columns and convert the Vols
                for (i in 1:N) { # Loops over rows
                        for (j in 2:M) { # Loops over columns
                                # Name of the "Expiry - Tenor" Identifier
                                Name <-
                                        paste0(vols[i, 1],
                                                "-",
                                                names(vols)[j])

                                # Starting and Ending Date of the underlying Swap
                                StartDate <-
                                        roll_month(Date = valdate,
                                                Offset = 12 * vols[i, 1]) # Option Expiry in MONTHS
                                StartDate <-
                                        parse_date_internal(DateToParse = roll_weekday(Day = StartDate,
                                                BusDayConv = "mf"), # Business Day Convention
                                                DateType = "European")
                                StartDate <-
                                        offset(dates = StartDate,
                                                n = 2,
                                                cal = "Actual") # Spot lag of 2 days from Option expiry
                                EndDate <-
                                        parse_date_internal(
                                                DateToParse = roll_month(Date = StartDate,
                                                        Offset = 12 * as.numeric(names(vols)[j])),
                                                DateType = "European"
                                        )

                                Dates[[Name]] <-
                                        generate_dates(
                                                StartDate = StartDate,
                                                EndDate = EndDate,
                                                ValDate = valdate,
                                                CouponFreq = TenorString,
                                                BusDayConv = "mf",
                                                Output = "Frame"
                                        )


                                # Annuity & Fwd Swap Rate
                                # NameOIS <-
                                #         substr(
                                #                 x = names(vols)[1],
                                #                 start = 1,
                                #                 stop = 3
                                #         )
                                # NameDSC <-
                                #         paste0(
                                #                 substr(
                                #                         x = names(vols)[1],
                                #                         start = 1,
                                #                         stop = 3
                                #                 ),
                                #                 ".",
                                #                 substr(
                                #                         x = names(vols)[1],
                                #                         start = 4,
                                #                         stop = 4
                                #                 ),
                                #                 "M"
                                #         )

                                Output <-
                                        forward_swap_rate(
                                                StartDate = StartDate,
                                                EndDate = EndDate,
                                                ValDate = valdate,
                                                cvDiscount = cvDiscount,
                                                cvTenor = cvTenor,
                                                FloatingFreq = TenorString
                                        )

                                ATMStrikes[i, j] <- Output[[1]] # ATM Strike is the Forward Swap Rate
                                Annuity[i, j] <- Output[[2]]

                                # Calculate Swaption Price
                                Prices[i, j] <-
                                        value_swaption(
                                                FwdRate = Output[[1]],
                                                Strike = Output[[1]], # All ATM Swaptions
                                                Vol = vols[i, j],
                                                T = as.numeric(vols[i, 1]), # Options Expiry
                                                Annuity = Output[[2]],
                                                Notional = 1000000,
                                                Shift = 0,
                                                model = "BACH"
                                        )
                        }
                }

                # Format output
                ATMStrikes[, 1] <- vols[, 1]
                ATMStrikes <- as.data.frame(ATMStrikes)
                names(ATMStrikes) <- names(vols)
                Annuity[, 1] <- vols[, 1]
                Annuity <- as.data.frame(Annuity)
                names(Annuity) <- names(vols)
                Prices[, 1] <- vols[, 1]
                Prices <- as.data.frame(Prices)
                names(Prices) <- names(vols)

                Output <- list(vols, Prices, ATMStrikes, Annuity, Dates)
                names(Output) <- c("Vols", "Prices", "StrikesATM", "Annuities", "Dates")

                return(Output)
        }
