## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.align='center',
  comment = "#>"
)
library( ggplot2 )
library( simITS )

## ---- fig.align='center'------------------------------------------------------
data(mecklenberg)
head( mecklenberg )
meck = mutate( mecklenberg, pbail = 100 * pbail )
ggplot( meck, aes( x=month, y=pbail)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0.5, xmax=25, fill="lightgray"), col = "lightgray", alpha=0.25) +
  scale_fill_identity(name = "", guide = "none", labels = c('Post Policy era')) +
  geom_hline( yintercept = 0, col="black") +
  geom_line( col="black", lty=1, lwd=0.5) +
  geom_point() +
  scale_x_continuous( breaks = c(-29,-24,-18,-12,-6,1,6,12,18,24)) +    
  coord_cartesian(xlim=c(-29.5,24.5), ylim=c(0,100), expand=FALSE) +
  labs( title = " ", y = "Percent cases assigned bail", x = " " )

## -----------------------------------------------------------------------------
meck = add_lagged_covariates( meck, outcomename = "pbail", covariates=NULL )
sample_n( meck, 5 ) %>% arrange( month )

## -----------------------------------------------------------------------------
meck.pre = filter( meck, month <= 0 )    
mod = fit_model_default( meck.pre, "pbail" )
summary( mod )

## -----------------------------------------------------------------------------
t0 = 0
envelope = process_outcome_model( "pbail", meck, 
                                  t0=t0, R = 100, 
                                  summarize = TRUE, smooth=FALSE )
sample_n( envelope, 5 ) %>% arrange( month )

## -----------------------------------------------------------------------------
ggplot( envelope, aes( month ) ) +
  geom_line( aes( y=Y ), alpha = 0.6 ) +  # original data
  geom_point( aes( y=Y ) ) +              # original data
  geom_ribbon( aes( ymin=Ymin, ymax=Ymax ), alpha=0.2 ) +
  geom_line( aes( y = Ystar ), col="darkgrey" ) +
  geom_vline( xintercept = t0+0.5)

## ---- eval=FALSE--------------------------------------------------------------
#  make_envelope_graph(envelope, t0=t0)

## -----------------------------------------------------------------------------
predictions = process_outcome_model( "pbail", meck,
                                     t0=t0, R = 100,
                                     summarize = FALSE, smooth=FALSE )

## -----------------------------------------------------------------------------
sstat = aggregate_simulation_results( orig.data = meck, outcomename = "pbail",
                                      predictions = predictions, months = 1:18 )

quantile( sstat$t, c( 0.025, 0.975 ))
sstat$t.obs
sstat$t.obs - quantile( sstat$t, c( 0.025, 0.975 ))

## -----------------------------------------------------------------------------
dat = generate_fake_data( t_min=-60, t_max=18, t0 = 0 )
qplot( month, Y, data=dat, geom = c( "point","line") )

## ---- fig.width=7-------------------------------------------------------------
envelope = process_outcome_model( "Y", dat, t0=t0, R = 100, 
                                  summarize = TRUE, smooth=TRUE )
make_envelope_graph(envelope, t0 )

## ---- fig.width=7-------------------------------------------------------------
alphas = c( 6, 11, 20, 100 )
preds = purrr::map( alphas, function( alpha ) {
  pds = process_outcome_model( "Y", dat,
                               t0=t0, R = 20,
                               summarize = FALSE, smooth=TRUE,
                               smooth_k = alpha )
  pds
} )
names( preds ) = alphas
preds = bind_rows( preds, .id="alpha_k" )
ggplot( filter( preds, month >= t0 ), aes( month, Ysmooth ) ) +
  facet_wrap( ~ alpha_k ) +
  geom_line( aes( group=Run, col=alpha_k ), alpha=0.5, na.rm=TRUE) +
  geom_line( data=dat, aes( month, Y ), col="black", alpha=0.5 ) +
  geom_vline( xintercept=t0, col="red" ) +
  labs( x="month", y="proportion given bail")

## -----------------------------------------------------------------------------
data( newjersey )
fit_season_model_qtemp =  make_fit_season_model( ~ temperature + Q2 + Q3 + Q4 )

envelope = process_outcome_model( "n.warrant", newjersey, t0=-7, R = 100, 
                                  summarize = TRUE, smooth=TRUE, 
                                  fit_model = fit_season_model_qtemp )
make_envelope_graph( envelope, t0=-7 )

## -----------------------------------------------------------------------------
smoother = make_model_smoother( fit_model = fit_season_model_sin, covariates = newjersey )
envelope_sin = process_outcome_model( "n.warrant", newjersey, t0=-7, R = 100,
                                  summarize = TRUE, smooth=TRUE, smoother = smoother, smooth_k = 11,
                                  fit_model = fit_season_model_qtemp )
envelope_sin$Ysmooth.base = envelope$Ysmooth
envelope_sin$Ysmooth1.base = envelope$Ysmooth1
make_envelope_graph( filter( envelope_sin, month > -30 ), t0=-7 ) +
  geom_line( aes( y=Ysmooth.base ), col="blue", na.rm=TRUE ) +
  geom_line( aes( y=Ysmooth1.base ), col="blue", lty=2, na.rm=TRUE )

