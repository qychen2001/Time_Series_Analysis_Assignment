v24 = simARIMA(MA.para = 0.7, diff = 0, seq.length = 24)
v60 = simARIMA(MA.para = 0.7, diff = 0, seq.length = 60)
v120 = simARIMA(MA.para = 0.7, diff = 0, seq.length = 120)
rho = acfun(model = "MA", model.para = 0.7, lag.max = 1, plot = F)
r24 = acfun(data = v24, lag.max = 1, plot = F)
r60 = acfun(data = v60, lag.max = 1, plot = F)
r120 = acfun(data = v120, lag.max = 1, plot = F)

sim.25 = simARIMA(AR.para = 0.7, diff = 0, seq.length = 36)
theo.25 = acfun(model = "AR", model.para = 0.7, lag.max = 20)
samp.25 = acfun(data = sim.25, lag.max = 20)
theo.p.25 = pacfun(model = "AR", model.para = 0.7, lag.max = 20)
samp.p.25 = pacfun(data = sim.25, lag.max = 20)

sim.27 = simARIMA(AR.para = c(0.7, -0.4), diff = 0, seq.length = 72)
theo.27 = acfun(model = "AR", model.para = c(0.7, -0.4), lag.max = 20)
samp.27 = acfun(data = sim.27, lag.max = 20)
theo.p.27 = pacfun(model = "AR", model.para = c(0.7, -0.4), lag.max = 20)
samp.p.27 = pacfun(data = sim.27, lag.max = 20)

sim.28 = simARIMA(MA.para = c(0.7, -0.4), diff = 0, seq.length = 36)
theo.28 = acfun(model = "MA", model.para = c(0.7, -0.4), lag.max = 20)
samp.28 = acfun(data = sim.28, lag.max = 20)
theo.p.28 = pacfun(data = simARIMA(MA.para = c(0.7, -0.4),
                                   diff = 0, seq.length = 50000),
                   lag.max = 20)
samp.p.28 = pacfun(data = sim.28, lag.max = 20)

data("deere1")
ts33 = plot.ts(deere1, type = "o")
samp.33 = acfun(data = deere1, lag.max = 20)
deere1[which.max(deere1)] = (deere1[which.max(deere1)+1]+
  deere1[which.max(deere1)-1])/2
samp.d.33 = acfun(data = deere1.d, lag.max = 20)
samp.p.33 = pacfun(data = deere1.d, lag.max = 20)
