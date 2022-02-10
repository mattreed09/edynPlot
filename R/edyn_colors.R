#' get edyn colors
#'
#'
#'
#' @param n total number of colors to return
#' @param lightest starting value of lightness - defaults to zero to use default colors first
#' @param inc_bw should the returned colors include black and white
#'
#'
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(disp, hp, color = as.factor(carb) )) +
#' ggplot2::geom_point(size = 4) +
#' edyn_theme() +
#' ggplot2::scale_color_manual(values = edyn_colors(6, 0.1) )
#'
#' @import ggplot2
#' @import colorspace
#' @import purrr
#' @export


edyn_colors <- function(n = 8, lightest = 0, inc_bw = F, palette = 2)
{
  base_colors <- c('#C9D5C2','#F0EDE3','#E8CEC7','#FDF1BC','#CED5DD','#202A2C', '#000000','#FFFFFF')
  colors2 <- c('#c9d5c2', '#EC9F05', '#9F4A54', '#262730', '#4F7292', '#509EA5',  '#000000','#FFFFFF' )

  if(palette == 1)base_colors = base_colors
  else base_colors = colors2
  m=floor((n-1)/6)
  if(inc_bw)
      return(c(unlist(purrr::map(0.8*((lightest/0.8):(m))/(m+1), ~colorspace::darken(base_colors[1:6], .x ) ))[1:(n - 2)], '#111111','#EEEEEE'))
  return(c(unlist(purrr::map(0.8*((lightest/0.8):(m))/(m+1), ~colorspace::darken(base_colors[1:6], .x ) ))[1:(n)]))

  }


