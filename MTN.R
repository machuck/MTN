parse_ts<-function(vec_in) {
    require(anytime)
    s_no=vec_in[1]
    date=vec_in[2]
    ts=vec_in[3]
    #cat("parsing ",ts,"...\n")
    #cat("date: ",date,"\n")
    a<-parse_hhmm2(ts)
    a2<-cbind.data.frame(s_no,date=as.POSIXct(paste(anydate(as.numeric(date)),a$hm)),node=a$node)
    a2
}

is_hm<-function(hm) {
    ifelse(length(grep('[^0-9: ]',hm))==0,TRUE,FALSE)
}

parse_hhmm2<-function(hhmm_str) {
    hhmm_vec<-grep('^[A-Z]{4,}|[0-9][0-9]:',unlist(strsplit(x=gsub('[^0-9:A-Z]{1,}',' ',hhmm_str),' ')),val=T)
    cat("hhmm_str:",hhmm_str,"\n")
    cat("hhmm_vec",hhmm_vec,"\n")
    hm_idx<-grep('[0-9][0-9]:',hhmm_vec,val=F)
    node_idx<-grep('[0-9][0-9]:',hhmm_vec,val=F,invert=T)
    cat("hm_idx:",hm_idx,"\n")
    cat("node_idx:",node_idx,"\n")
    line_uped_idx<-hm_node_line_up2(hm_idx,node_idx)
    new_hm_idx<-line_uped_idx$p
    new_node_idx<-line_uped_idx$s
    cat("new_hm_idx:",new_hm_idx,"\nnew_node_idx:",new_node_idx,"\n")
    if(line_uped_idx$r) { hhmm_vec<-rev(hhmm_vec) }
    cbind.data.frame(hm=hhmm_vec[new_hm_idx],node=hhmm_vec[new_node_idx])
}

hm_node_line_up2<-function(primary,secondary) {
    reved=FALSE
    if(length(primary)==length(secondary))
        return(list(p=primary,s=secondary,r=reved))
    p=primary
    s=secondary
    if(length(p)>length(s)) {
        ds=c(diff(s)-1,p[length(p)]-s[length(s)])
        if(any(ds<0)) {
            cat("here...\n")
            tot_len<-length(c(p,s))
            p<-rev(tot_len-p+1)
            s<-rev(tot_len-s+1)
            cat("tot:",tot_len,"new p:",p,"new s:",s,"\n")
            ds=c(diff(s)-1,p[length(p)]-s[length(s)])
            cat("ds:",ds,"\n")
            reved=TRUE
        }
        s=unlist(apply(cbind(s,ds),1,function(x) rep(x[1],x[2])))
    } else {
        dp=c(diff(p)-1,s[length(s)]-p[length(p)])
        if(any(dp<0)) {
            tot_len<-length(c(p,s))
            p<-rev(tot_len-p+1)
            s<-rev(tot_len-s+1)
            dp=c(diff(p)-1,s[length(s)]-p[length(p)])
            reved=TRUE
        }
        p=unlist(apply(cbind(p,dp),1,function(x) rep(x[1],x[2])))
    }
    return(list(p=p,s=s,r=reved))
}
