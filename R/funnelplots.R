##funnelplots

#pain intensity

funnel_pintens_prpo <- rma(yi, vi, data=pintens_meta_prpo)
funnel_pintens_pof <- rma(yi, vi, data=pintens_meta_pof)
funnel_pintens_prf <- rma(yi, vi, data=pintens_meta_prf)

### set up 2x2 array for plotting
png(file="funnels_pintens.png", width = 600, height = 800)
par(mfrow=c(3,1))


### draw funnel plots

metafor::funnel(funnel_pintens_pof, main="pintens pre-post, Standard Error")
metafor::funnel(funnel_pintens_prf, main="pintens post-fu, Standard Error")
metafor::funnel(funnel_pintens_prpo, main="pintens pre-fu, Standard Error")

dev.off()

#physical function

funnel_pf_prpo <- rma(yi, vi, data=pf_meta_prpo)
funnel_pf_pof <- rma(yi, vi, data=pf_meta_pof)
funnel_pf_prf <- rma(yi, vi, data=pf_meta_prf)

### set up 2x2 array for plotting
png(file="funnels_pf.png", width = 600, height = 800)
par(mfrow=c(3,1))


### draw funnel plots

metafor::funnel(funnel_pf_pof, main="pf pre-post, Standard Error")
metafor::funnel(funnel_pf_prf, main="pf post-fu, Standard Error")
metafor::funnel(funnel_pf_prpo, main="pf pre-fu, Standard Error")


dev.off()

#pain interference

funnel_pinter_prpo <- rma(yi, vi, data=pinter_meta_prpo)
funnel_pinter_pof <- rma(yi, vi, data=pinter_meta_pof)
funnel_pinter_prf <- rma(yi, vi, data=pinter_meta_prf)

### set up 2x2 array for plotting
png(file="funnels_pinter.png", width = 600, height = 800)
par(mfrow=c(3,1))


### draw funnel plots

metafor::funnel(funnel_pinter_pof, main="pinter pre-post, Standard Error")
metafor::funnel(funnel_pinter_prf, main="pinter post-fu, Standard Error")
metafor::funnel(funnel_pinter_prpo, main="pinter pre-fu, Standard Error")

dev.off()

#depression

funnel_dep_prpo <- rma(yi, vi, data=dep_meta_prpo)
funnel_dep_pof <- rma(yi, vi, data=dep_meta_pof)
funnel_dep_prf <- rma(yi, vi, data=dep_meta_prf)

### set up 2x2 array for plotting
png(file="funnels_dep.png", width = 600, height = 800)
par(mfrow=c(3,1))


### draw funnel plots

metafor::funnel(funnel_dep_pof, main="dep pre-post, Standard Error")
metafor::funnel(funnel_dep_prf, main="dep post-fu, Standard Error")
metafor::funnel(funnel_dep_prpo, main="dep pre-fu, Standard Error")

dev.off()

#anxiety

funnel_anx_prpo <- rma(yi, vi, data=anx_meta_prpo)
funnel_anx_pof <- rma(yi, vi, data=anx_meta_pof)
funnel_anx_prf <- rma(yi, vi, data=anx_meta_prf)

### set up 2x2 array for plotting
png(file="funnels_anx.png", width = 600, height = 800)
par(mfrow=c(3,1))


### draw funnel plots

metafor::funnel(funnel_anx_pof, main="anx pre-post, Standard Error")
metafor::funnel(funnel_anx_prf, main="anx post-fu, Standard Error")
metafor::funnel(funnel_anx_prpo, main="anx pre-fu, Standard Error")

dev.off()

#self-efficacy

funnel_se_prpo <- rma(yi, vi, data=se_meta_prpo)
funnel_se_pof <- rma(yi, vi, data=se_meta_pof)
funnel_se_prf <- rma(yi, vi, data=se_meta_prf)

### set up 2x2 array for plotting
png(file="funnels_se.png", width = 600, height = 800)
par(mfrow=c(3,1))


### draw funnel plots

metafor::funnel(funnel_se_pof, main="se pre-post, Standard Error")
metafor::funnel(funnel_se_prf, main="se post-fu, Standard Error")
metafor::funnel(funnel_se_prpo, main="se pre-fu, Standard Error")

dev.off()

#general emotional functioning

funnel_ef_prpo <- rma(yi, vi, data=ef_meta_prpo)
funnel_ef_pof <- rma(yi, vi, data=ef_meta_pof)
funnel_ef_prf <- rma(yi, vi, data=ef_meta_prf)

### set up 2x2 array for plotting
png(file="funnels_ef.png", width = 600, height = 800)
par(mfrow=c(3,1))


### draw funnel plots

metafor::funnel(funnel_ef_pof, main="ef pre-post, Standard Error")
metafor::funnel(funnel_ef_prf, main="ef post-fu, Standard Error")
metafor::funnel(funnel_ef_prpo, main="ef pre-fu, Standard Error")

dev.off()

#anger

funnel_ang_prpo <- rma(yi, vi, data=ang_meta_prpo)
funnel_ang_pof <- rma(yi, vi, data=ang_meta_pof)
funnel_ang_prf <- rma(yi, vi, data=ang_meta_prf)

### set up 2x2 array for plotting
png(file="funnels_ang.png", width = 600, height = 800)
par(mfrow=c(3,1))


### draw funnel plots

metafor::funnel(funnel_ang_pof, main="ang pre-post, Standard Error")
metafor::funnel(funnel_ang_prf, main="ang post-fu, Standard Error")
metafor::funnel(funnel_ang_prpo, main="ang pre-fu, Standard Error")

dev.off()

#social role functioning

funnel_srf_prpo <- rma(yi, vi, data=srf_meta_prpo)
funnel_srf_pof <- rma(yi, vi, data=srf_meta_pof)
funnel_srf_prf <- rma(yi, vi, data=srf_meta_prf)

### set up 2x2 array for plotting
png(file="funnels_srf.png", width = 600, height = 800)
par(mfrow=c(3,1))


### draw funnel plots

metafor::funnel(funnel_srf_pof, main="srf pre-post, Standard Error")
metafor::funnel(funnel_srf_prf, main="srf post-fu, Standard Error")
metafor::funnel(funnel_srf_prpo, main="srf pre-fu, Standard Error")

dev.off()
