\name{force.colour.scheme}
\alias{force.colour.scheme}
\title{Based on predefined colour schemes, returns a vector of corresponding colours.}
\description{Takes a vector of character strings and an scheme returns the matching colours as a vector.}
\usage{
force.colour.scheme(x, scheme, fill.colour = NA, include.names = FALSE, 
return.factor = FALSE, return.scheme = FALSE)
}
\arguments{
    \item{x}{The input character or numeric vector}
    \item{scheme}{A string representing a predefined scheme. Available schemes are: 
        \dQuote{annovar.annotation}, \dQuote{annovar.annotation.collapsed}, 
        \dQuote{annovar.annotation.collapsed2}, \dQuote{tissue}, \dQuote{sex}, \dQuote{stage}, 
        \dQuote{risk}, \dQuote{MSI}, \dQuote{tumour}, \dQuote{CNV}, \dQuote{organism}},
        \dQuote{chromosome} and \dQuote{biomolecule}
    \item{fill.colour}{Value to enter when value of x not present in scheme.}
    \item{include.names}{bool: whether to output a named vector or not}
    \item{return.factor}{Alt behaviour to return factors, not colours}
    \item{return.scheme}{Alt behaviour to return scheme list, not colours}
    }

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

    } 

\author{Nicholas Harding}
\examples{
annovar.output <- c('nonsynonymous snv', 'stopgain snv', 'none', 'stoploss snv', 
'frameshift deletion', 'frameshift substitution', 'splicing', 'none');
force.colour.scheme(annovar.output,'annovar.annotation');
force.colour.scheme(annovar.output,'annovar.annotation', 'white');
}
