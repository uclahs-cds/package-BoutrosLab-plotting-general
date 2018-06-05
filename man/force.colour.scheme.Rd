\name{force.colour.scheme}
\alias{force.colour.scheme}
\alias{force.color.scheme}
\title{Based on predefined colour schemes, returns a vector of corresponding colours.}
\description{Takes a vector of character strings and an scheme returns the matching colours as a vector.}
\usage{
force.colour.scheme(
	x = NA,
	scheme,
	fill.colour = 'slategrey',
	include.names = FALSE,
	return.factor = FALSE,
	return.scheme = FALSE
	);
}
\arguments{
    \item{x}{The input character or numeric vector, defaults to NA incase return.scheme = TRUE.}
    \item{scheme}{A string representing a predefined scheme. Available schemes are: 
        \dQuote{annovar.annotation}, \dQuote{annovar.annotation.collapsed}, 
        \dQuote{annovar.annotation.collapsed2}, \dQuote{tissue}, \dQuote{sex}, \dQuote{stage}, 
        \dQuote{risk}, \dQuote{MSI}, \dQuote{tumour}, \dQuote{CNV}, \dQuote{organism}},
        \dQuote{chromosome} and \dQuote{biomolecule}
    \item{fill.colour}{Value to enter when value of x not present in scheme.}
    \item{include.names}{Should the output be a named vector or not?}
    \item{return.factor}{Should factors (scheme names) be returned?}
    \item{return.scheme}{Should the scheme list be returned?}
    }
\value{If multiple returns are requested, outputs a list (return.factor: factor length x with scheme names; scheme: list containing scheme names and colours; colours: vector length x with the required colours).}
\details{
    The input character options for each colour scheme are as follows:
    annovar.annotation
    \itemize{
        \item{nonsynonymous snv}
        \item{stopgain snv}
        \item{stoploss snv}
        \item{frameshift deletion}
        \item{frameshift substitution}
        \item{splicing}
        \item{synonymous snv}
        }

    annovar.annotation.collapsed
    \itemize{
        \item{nonsynonymous snv}
        \item{stopgain snv}
        \item{stoploss SNV}
        \item{frameshift indel}
        \item{splicing}
        }  

    annovar.annotation.collapsed2
    \itemize{
        \item{nonsynonymous}
        \item{stopgain-stoploss}
        \item{splicing}
        \item{frameshift indel} 
        \item{synonymous}
        \item{utr5-utr3}
        \item{nonframeshift indel}
        \item{intronic}
        \item{intergenic}
        \item{other}
        }  

    tissue
    \itemize{
        \item{cartilage}
        \item{bone}
        \item{adipose}
        \item{bladder}
        \item{kidney}
        \item{blood}
        \item{heart}
        \item{muscle}
        \item{hypothalamus}
        \item{pituitary}
        \item{thyroid}
        \item{parathyroid}
        \item{skin}
        \item{salivarygland}
        \item{esophagus}
        \item{stomach}
        \item{liver}
        \item{gallbladder}
        \item{pancreas}
        \item{intestine}
        \item{colon}
        \item{pharynx}
        \item{larynx}
        \item{trachea}
        \item{diaphragm}
        \item{lung}
        \item{nerve}
        \item{spine}
        \item{brain}
        \item{eye}
        \item{breast}
        \item{ovary}
        \item{uterus}
        \item{prostate}
        \item{testes}
        \item{lymph}
        \item{leukocyte}
        \item{spleen}
        }
    
    sex
    \itemize{
        \item{male}
        \item{female}
        }
    
    stage
    \itemize{
        \item{I}
        \item{II}
        \item{III}
        \item{IV}
        }
    
    risk
    \itemize{
        \item{High}
        \item{Low}
        }
    
    MSI
    \itemize{
        \item{MSI-High}
        \item{MSI-Low}
        \item{MSS}
        }
    
    tumour
    \itemize{
        \item{Primary}
        \item{Metastatic}
        }
    
    CNV
    \itemize{
        \item{Amplification}
        \item{Deletion}
        \item{LOH}
        \item{Neutral}
        }
    
    organism
    \itemize{
        \item{Human}
        \item{Rat}
        \item{Mouse}
        }

    chromosome
    \itemize{
        \item{1 - 22}
        \item{X}
        \item{Y}
        }

    biomolecule
    \itemize{
        \item{DNA}
        \item{RNA}
        \item{Protein}
        \item{Carbohydrate}
        \item{Lipid}
        }

    clinicalt3
    \itemize{
        \item{t0}
        \item{t1}
        \item{t2}
        \item{t3}
        \item{t4}
        \item{t5}
        }

    clinicalt9
    \itemize{
        \item{t1a}
        \item{t1b}
        \item{t1c}
        \item{t2a}
        \item{t2b}
        \item{t2c}
        \item{t3a}
        \item{t3b}
        \item{t3c}
        }

    gleason.score
    \itemize{
        \item{3+3}
        \item{3+4}
        \item{4+4}
        \item{4+5}
        \item{3+5}
        \item{5+3}
        \item{5+4}
        \item{5+5}
        \item{missing}
        \item{NA}
        }

    gleason.sum
    \itemize{
        \item{5}
        \item{6}
        \item{7}
        \item{8}
        \item{9}
        \item{missing}
        \item{NA}
        }

    tissue.color
    \itemize{
        \item{blood}
        \item{frozen}
        \item{ffpe}
        }

    psa.categorical
    \itemize{
        \item{0 - 9.9}
        \item{10 - 19.9}
        \item{>= 20}
        }
  
    age.categorical.default
    \itemize{
        \item{<50}
        \item{50 - 60}
        \item{60 - 70}
        \item{>= 70}
        }

    age.categorical.prostate
    \itemize{
        \item{<40}
        \item{40 - 50}
        \item{50 - 65}
        \item{65 - 70}
        \item{>= 70}
        }
    age.gradient

    psa.gradient 

    heteroplasmy
    \itemize{
	\item{0 - 0.2}
        \item{0.2 - 0.4}
        \item{0.4 - 0.6}
        \item{0.6 - 1.0}
        }
    
    mt.annotation
    \itemize{
        \item{MT-DLOOP}
        \item{MT-T*}
        \item{MT-RNR*}
        \item{MT-ND1}
        \item{MT-ND2}
        \item{MT-ND3}
        \item{MT-ND4L}
        \item{MT-ND4L/MT-ND4}
        \item{MT-ND4}
        \item{MT-ND5}
        \item{MT-ND6}
        \item{MT-CO1}
        \item{MT-CO2}
        \item{MT-CO3}
        \item{MT-ATP6/MT-CO3}
        \item{MT-ATP6}
        \item{MT-ATP8/MT-ATP6}
        \item{MT-ATP8}
        \item{MT-CYB}
        \item{MT-NC*}
        \item{MT-OL*}
        }

    isup.grade 
    \itemize{
        \item{1}
        \item{2}
        \item{3}
        \item{4}
        \item{5} 
    }

    

  } 

\author{Nicholas Harding}
\examples{
annovar.output <- c('nonsynonymous snv', 'stopgain snv', 'none', 'stoploss snv', 
'frameshift deletion', 'frameshift substitution', 'splicing', 'none');
force.colour.scheme(annovar.output,'annovar.annotation');
force.colour.scheme(annovar.output,'annovar.annotation', 'white');
}
