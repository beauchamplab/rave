rad_to_deg <- function(rad) rad*180/pi

.build_circle <- function(radius, res) {
    theta <- seq(0,2*pi, length.out = res)
    
    x <- radius*cos(theta)
    y <- radius*sin(theta)
    
    cbind(x,y)
}

draw.circle <- function(center, radius, res=100, col='gray30', ...) {
    if(missing(center))center <- c(0,0)
    
    circ <- .build_circle(radius, res) %>% t %>% add(center) %>% t
    lines(circ, col=col, ...)
}

fill.circle <- function(center, radius, res=100, col='gray80', border=NA) {
    if(missing(center))center <- c(0,0)
    
    circ <- .build_circle(radius, res) %>% t %>% add(center) %>% t
    polygon(circ[,1], circ[,2], col='gray80', border=border)
    
    invisible(circ)
}

draw.line <- function(from,to, ...) {
    segments(from[1], from[2], to[1], to[2], ...)
}

draw.arrow <- function(from,to, length=.1, col='gray30', ...) {
    arrows(from[1], from[2], to[1], to[2], length=length, col=col, ...)
}

labeled_circle <- function(center, radius, label) {
    draw.circle(center, radius)
    text(center[1], center[2], label)
}

generate_FROM_angles <- function(n_from) {
    seq(-pi,pi, length.out = n_from+1)[seq_len(n_from)]
}

# i thought this could be solved with a simpler %%, but my geometry is :(
fix_rotation <- function(rt) {
    if(rt < -90) return (180-abs(rt))
    
    if(rt > 90) return (rt-180)
    
    return (rt)
}

plot_connectivity <- function(from_names, to_name, main='', upper_labels='', lower_labels='',
                              lwd=1, lty=1, col='gray30',
                              half_window=30, radius=4.5, gap_factor=5, asp=1,
                              draw_bounding_circle=TRUE, draw_trial_label=TRUE) {
    n_from <- length(from_names)
    
    plot.clean(-half_window:half_window, -half_window:half_window, asp=asp)
    
    thetas <- generate_FROM_angles(n_from)
    
    labeled_circle(center=c(0,0), radius = radius, label=to_name)    
    
    repN <- function(y) rep(y, length.out=n_from)
    
    lwd %<>% repN
    lty %<>% repN
    col %<>% repN
    
    upper_labels %<>% repN
    lower_labels %<>% repN
    
    for(ii in seq_along(thetas)) {
        th <- thetas[ii]
        cth_sth <- c(cos(th), sin(th))
        
        center = c(gap_factor * radius) * cth_sth
        labeled_circle(center=center, radius=radius, label=from_names[ii])        
        
        # for the arrows, we want to draw them along the line
        # that connects the center of FROM and TO (which is 0,0)
        .from <- radius*(gap_factor - 1) * cth_sth
        .to <- radius*cth_sth
        
        draw.arrow(from=.from, to=.to, col=col[ii], lwd=lwd[ii], lty=lty[ii],
                   length = .075)
        
        #half-way between the circles is where we should draw the labels
        
        # the uncondtional correlation
        text(rowMeans(cbind(.from, .to)) %>% t, lower_labels[ii],
             srt=fix_rotation(rad_to_deg(th)),
             adj=c(0.5, 1.25), col='gray30', cex=1.2)
        
        # the condtional correlation
        text(rowMeans(cbind(.from, .to)) %>% t, upper_labels[ii],
             srt=fix_rotation(rad_to_deg(th)),
             adj=c(0.5, -.25), col='orange', cex=1.2)
    }
    
    if(draw_bounding_circle) {
        draw.circle(radius=half_window)
    }
    
    if(draw_trial_label) {
        title(main=main)
    }
}


if(FALSE) {
    # text(-13, 14, label='HI', srt=rad_to_deg(th))
    par(mfrow=c(1,3), mar=c(0,0,2,0))
    
    ## the prefix ends with ._ so just look for that
    strip_prfx <- function(s, prfx='\\._') {
        s %>% str_split(prfx) %>% unlist %>% tail(1)
    }
    
    get_pcor <- function(ll) format(ll['partial_cor'], digits=2)
    get_uncond_cor <- function(ll) format(ll['marginal_cor'], digits=2)
    
    par(mfrow=c(1,3))
    mapply(function(ll, nm) {
        # if there is more than one TO electrode, do separate circles here
        mapply(function(li, to_nm) {
            layout_circles(length(li), names(li) %>% sapply(strip_prfx, '\\.'),
                           to_name=to_nm, main=nm,
                           upper_labels=li %>% sapply(get_pcor),
                           lower_labels=li %>% sapply(get_uncond_cor))
            
        }, ll, names(ll) %>% sapply(strip_prfx, '\\.'))
    }, res, names(res) %>% sapply(strip_prfx))
    
    
    for(ii in 1:4) layout_circles(7,"F_" %&% 1:7, 'To', main='Trial Type ' %&% ii)
    
    2*pi * 180/pi
    
    
    layout_circles(3, 1:3, 'a')
    
    rad_to_deg(generate_FROM_angles(3))[3]
}





