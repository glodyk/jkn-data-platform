setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")

# Core packages
library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)
library(tidyquant)
library(stringr)
library(lubridate)
library(randomForest)
library(styler)
library(splitstackshape)
library(sos)
library(readr)
library(anytime)
library(styler)
library(openxlsx)
library(data.table)

load("ur_all.rda")
data <- data %>%
  select(-CMG,-CBG,-Spec,-Sevel,-tipe)

nonspes <- data %>% subset(Tglpelayanan >= "2019-01-01") %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"A010|A059|A060|A150|A184|A270|A300|A35|A370|A46|A510|A520|A540|A90|A91|A920|B001|B009|B019|B029|B059|B07|B081|B150|B160|B170|B171|B172|B24|B260|B350|B351|B352|B353|B354|B356|B359|B360|B500|B510|B520|B530|B54|B650|B680|B740|B760|B769|B770|B780|B850|B853|B86|D179|D500|D649|E100|E110|E162|E46|E569|E610|E660|E740|E785|E790|F450|G430|G442|G470|G510|H000|H010|H020|H100|H113|H151|H162|H520|H521|H522|H524|H531|H600|H612|H669|H810|I10|I839|I840|I880|J00|J020|J030|J040|J100|J120|J300|J304|J340|J40|J459|K210|K291|K297|K30|K529|L010|L020|L022|L028|L080|L081|L089|L200|L210|L240|L270|L271|L272|L300|L309|L42|L500|L700|L710|L732|L743|N12|N390|N47|N508|N61|N645|N700|N760|N762|N768|O000|O030|O25|O700|O701|O702|O703|O800|O801|O808|O809|O839|O921|R040|R560|R51|T151|T171|T301|T302|T753|T782|T812|Z719|Z370")) %>%
  filter(!str_detect(Diagnosa,"H52"))

nonspes <- nonspes %>%
  mutate(
    Kddiagprimer1 = case_when(
      Kddiagprimer == "Z031" ~ NA,
      Kddiagprimer == "Z039" ~ NA,
      Kddiagprimer == "Z041" ~ NA,
      Kddiagprimer == "Z291" ~ NA,
      Kddiagprimer == "Z340" ~ NA,
      Kddiagprimer == "Z349" ~ NA,
      Kddiagprimer == "Z358" ~ NA,
      Kddiagprimer == "Z359" ~ NA,
      Kddiagprimer == "Z370" ~ NA,
      Kddiagprimer == "Z390" ~ NA,
      Kddiagprimer == "Z392" ~ NA,
      Kddiagprimer == "Z419" ~ NA,
      Kddiagprimer == "Z429" ~ NA,
      Kddiagprimer == "Z488" ~ NA,
      Kddiagprimer == "Z489" ~ NA,
      Kddiagprimer == "Z490" ~ NA,
      Kddiagprimer == "Z491" ~ NA,
      Kddiagprimer == "Z492" ~ NA,
      Kddiagprimer == "Z941" ~ NA,
      Kddiagprimer == "Z501" ~ NA,
      Kddiagprimer == "Z504" ~ NA,
      Kddiagprimer == "Z505" ~ NA,
      Kddiagprimer == "Z507" ~ NA,
      Kddiagprimer == "Z509" ~ NA,
      Kddiagprimer == "Z511" ~ NA,
      Kddiagprimer == "Z549" ~ NA,
      Kddiagprimer == "Z719" ~ NA,
      Kddiagprimer == "Z088" ~ NA,
      Kddiagprimer == "Z089" ~ NA,
      Kddiagprimer == "Z090" ~ NA,
      Kddiagprimer == "Z091" ~ NA,
      Kddiagprimer == "Z092" ~ NA,
      Kddiagprimer == "Z093" ~ NA,
      Kddiagprimer == "Z094" ~ NA,
      Kddiagprimer == "Z095" ~ NA,
      Kddiagprimer == "Z096" ~ NA,
      Kddiagprimer == "Z097" ~ NA,
      Kddiagprimer == "Z098" ~ NA,
      Kddiagprimer == "Z099" ~ NA,
      Kddiagprimer == "Z898" ~ NA,
      Kddiagprimer == "Z908" ~ NA,
      Kddiagprimer == "Z961" ~ NA,
      Kddiagprimer == "Z988" ~ NA,
      TRUE ~ Kddiagprimer))
nonspes$Nmdiagprimer1 <- with(nonspes,
                              ifelse(is.na(nonspes$Kddiagprimer1),NA,Nmdiagprimer))
nonspes <- nonspes %>%
  mutate(Diagprimer = paste0(as.character(Kddiagprimer1)," - ",
                             as.character(Nmdiagprimer1)))
nonspes$Diagprimer <- as.character(trimws(gsub("NA - NA",NA,nonspes$Diagprimer)), "both")
nonspes <- nonspes %>%
  mutate(Diagnosa = paste0(as.character(Diagprimer),";",
                           as.character(Diagsekunder)))
nonspes$Diagnosa <- as.character(trimws(gsub(";NA|NA;|NA","",nonspes$Diagnosa)), "both")
nonspes <- nonspes %>% select(-Kddiagprimer1,-Nmdiagprimer1,-Diagprimer)

library(splitstackshape)
nonspes <- concat.split(nonspes, "Diagnosa", ";")
colnames(nonspes)

nonspes$Diagnosa_01 <- as.character(trimws(nonspes$Diagnosa_01))
nonspes$Diagnosa_02 <- as.character(trimws(nonspes$Diagnosa_02))
nonspes$Diagnosa_03 <- as.character(trimws(nonspes$Diagnosa_03))
nonspes$Diagnosa_04 <- as.character(trimws(nonspes$Diagnosa_04))
nonspes$Diagnosa_05 <- as.character(trimws(nonspes$Diagnosa_05))
nonspes$Diagnosa_06 <- as.character(trimws(nonspes$Diagnosa_06))
nonspes$Diagnosa_07 <- as.character(trimws(nonspes$Diagnosa_07))
nonspes$Diagnosa_08 <- as.character(trimws(nonspes$Diagnosa_08))
nonspes$Diagnosa_09 <- as.character(trimws(nonspes$Diagnosa_09))
nonspes$Diagnosa_10 <- as.character(trimws(nonspes$Diagnosa_10))
nonspes$Diagnosa_11 <- as.character(trimws(nonspes$Diagnosa_11))
nonspes$Diagnosa_12 <- as.character(trimws(nonspes$Diagnosa_12))
nonspes$Diagnosa_13 <- as.character(trimws(nonspes$Diagnosa_13))

nonspes$Diagnosa_01 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_01),NA,Diagnosa_01))
nonspes$Diagnosa_02 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_02),NA,Diagnosa_02))
nonspes$Diagnosa_03 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_03),NA,Diagnosa_03))
nonspes$Diagnosa_04 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_04),NA,Diagnosa_04))
nonspes$Diagnosa_05 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_05),NA,Diagnosa_05))
nonspes$Diagnosa_06 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_06),NA,Diagnosa_06))
nonspes$Diagnosa_07 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_07),NA,Diagnosa_07))
nonspes$Diagnosa_08 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_08),NA,Diagnosa_08))
nonspes$Diagnosa_09 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_09),NA,Diagnosa_09))
nonspes$Diagnosa_10 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_10),NA,Diagnosa_10))
nonspes$Diagnosa_11 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_11),NA,Diagnosa_11))
nonspes$Diagnosa_12 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_12),NA,Diagnosa_12))
nonspes$Diagnosa_13 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_13),NA,Diagnosa_13))

nonspes <- nonspes %>%
  mutate(
    Diagnosa_1 = case_when(
      Diagnosa_01 == "A010 - Typhoid fever" ~ NA,
      Diagnosa_01 == "A059 - Bacterial foodborne intoxication, unspecified" ~ NA,
      Diagnosa_01 == "A060 - Acute amoebic dysentery" ~ NA,
      Diagnosa_01 == "A150 - Tuberculosis of lung, confirmed by sputum microscopy with or without culture" ~ NA,
      Diagnosa_01 == "A184 - Tuberculosis of skin and subcutaneous tissue" ~ NA,
      Diagnosa_01 == "A270 - Leptospirosis icterohaemorrhagica" ~ NA,
      Diagnosa_01 == "A300 - Indeterminate leprosy" ~ NA,
      Diagnosa_01 == "A35 - Other tetanus" ~ NA,
      Diagnosa_01 == "A370 - Whooping cough due to Bordetella pertussis" ~ NA,
      Diagnosa_01 == "A46 - Erysipelas" ~ NA,
      Diagnosa_01 == "A510 - Primary genital syphilis" ~ NA,
      Diagnosa_01 == "A520 - Cardiovascular syphilis" ~ NA,
      Diagnosa_01 == "A540 - nococcal infection of lower genitourinary tract without periurethral or accessory gland abscess" ~ NA,
      Diagnosa_01 == "A90 - Dengue fever [classical dengue]" ~ NA,
      Diagnosa_01 == "A91 - Dengue haemorrhagic fever" ~ NA,
      Diagnosa_01 == "A920 - Chikungunya virus disease" ~ NA,
      Diagnosa_01 == "B001 - Herpesviral vesicular dermatitis" ~ NA,
      Diagnosa_01 == "B009 - Herpesviral infection, unspecified" ~ NA,
      Diagnosa_01 == "B019 - Varicella without complication" ~ NA,
      Diagnosa_01 == "B029 - Zoster without complication" ~ NA,
      Diagnosa_01 == "B059 - Measles without complication" ~ NA,
      Diagnosa_01 == "B07 - Viral warts" ~ NA,
      Diagnosa_01 == "B081 - Molluscum contagiosum" ~ NA,
      Diagnosa_01 == "B150 - Hepatitis A with hepatic coma" ~ NA,
      Diagnosa_01 == "B160 - Acute hepatitis B with delta-agent (coinfection) with hepatic coma" ~ NA,
      Diagnosa_01 == "B170 - Acute delta-(super)infection of hepatitis B carrier" ~ NA,
      Diagnosa_01 == "B171 - Acute hepatitis C" ~ NA,
      Diagnosa_01 == "B172 - Acute hepatitis E" ~ NA,
      Diagnosa_01 == "B24 - Unspecified human immunodeficiency virus [HIV] disease" ~ NA,
      Diagnosa_01 == "B260 - Mumps orchitis" ~ NA,
      Diagnosa_01 == "B350 - Tinea barbae and tinea capitis" ~ NA,
      Diagnosa_01 == "B351 - Tinea unguium" ~ NA,
      Diagnosa_01 == "B352 - Tinea manuum" ~ NA,
      Diagnosa_01 == "B353 - Tinea pedis" ~ NA,
      Diagnosa_01 == "B354 - Tinea corporis" ~ NA,
      Diagnosa_01 == "B356 - Tinea cruris" ~ NA,
      Diagnosa_01 == "B359 - Dermatophytosis, unspecified" ~ NA,
      Diagnosa_01 == "B360 - Pityriasis versicolor" ~ NA,
      Diagnosa_01 == "B500 - Plasmodium falciparum malaria with cerebral complications" ~ NA,
      Diagnosa_01 == "B510 - Plasmodium vivax malaria with rupture of spleen" ~ NA,
      Diagnosa_01 == "B520 - Plasmodium malariae malaria with nephropathy" ~ NA,
      Diagnosa_01 == "B530 - Plasmodium ovale malaria" ~ NA,
      Diagnosa_01 == "B54 - Unspecified malaria" ~ NA,
      Diagnosa_01 == "B650 - Schistosomiasis due to Schistosoma haematobium [urinary schistosomiasis]" ~ NA,
      Diagnosa_01 == "B680 - Von Willebrand disease" ~ NA,
      Diagnosa_01 == "B740 - Filariasis due to Wuchereria bancrofti" ~ NA,
      Diagnosa_01 == "B760 - Ancylostomiasis" ~ NA,
      Diagnosa_01 == "B769 - Hookworm disease, unspecified" ~ NA,
      Diagnosa_01 == "B770 - Ascariasis with intestinal complications" ~ NA,
      Diagnosa_01 == "B780 - Intestinal strongyloidiasis" ~ NA,
      Diagnosa_01 == "B850 - Pediculosis due to Pediculus humanus capitis" ~ NA,
      Diagnosa_01 == "B853 - Phthiriasis" ~ NA,
      Diagnosa_01 == "B86 - Scabies" ~ NA,
      Diagnosa_01 == "D179 - Benign lipomatous neoplasm, unspecified" ~ NA,
      Diagnosa_01 == "D500 - Iron deficiency anaemia secondary to blood loss (chronic)" ~ NA,
      Diagnosa_01 == "D649 - Anaemia, unspecified" ~ NA,
      Diagnosa_01 == "E100 - Insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_01 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_01 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ NA,
      Diagnosa_01 == "E162 - Hypoglycaemia, unspecified" ~ NA,
      Diagnosa_01 == "E46 - Unspecified protein-energy malnutrition" ~ NA,
      Diagnosa_01 == "E569 - Vitamin deficiency, unspecified" ~ NA,
      Diagnosa_01 == "E610 - Copper deficiency" ~ NA,
      Diagnosa_01 == "E660 - Obesity due to excess calories" ~ NA,
      Diagnosa_01 == "E740 - Glycogen storage disease" ~ NA,
      Diagnosa_01 == "E785 - Hyperlipidaemia, unspecified" ~ NA,
      Diagnosa_01 == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease" ~ NA,
      Diagnosa_01 == "F450 - Somatization disorder" ~ NA,
      Diagnosa_01 == "G430 - Migraine without aura [common migraine]" ~ NA,
      Diagnosa_01 == "G442 - Tension-type headache" ~ NA,
      Diagnosa_01 == "G470 - Disorders of initiating and maintaining sleep [insomnias]" ~ NA,
      Diagnosa_01 == "G510 - Bell's palsy" ~ NA,
      Diagnosa_01 == "H000 - Hordeolum and other deep inflammation of eyelid" ~ NA,
      Diagnosa_01 == "H010 - Blepharitis" ~ NA,
      Diagnosa_01 == "H020 - Entropion and trichiasis of eyelid" ~ NA,
      Diagnosa_01 == "H100 - Mucopurulent conjunctivitis" ~ NA,
      Diagnosa_01 == "H113 - Conjunctival haemorrhage" ~ NA,
      Diagnosa_01 == "H151 - Episcleritis" ~ NA,
      Diagnosa_01 == "H162 - Keratoconjunctivitis" ~ NA,
      Diagnosa_01 == "H524 - Presbyopia" ~ NA,
      Diagnosa_01 == "H531 - Subjective visual disturbances" ~ NA,
      Diagnosa_01 == "H600 - Abscess of external ear" ~ NA,
      Diagnosa_01 == "H612 - Impacted cerumen" ~ NA,
      Diagnosa_01 == "H669 - Otitis media, unspecified" ~ NA,
      Diagnosa_01 == "H810 - Meniere's disease" ~ NA,
      Diagnosa_01 == "I10 - Essential (primary) hypertension" ~ NA,
      Diagnosa_01 == "I839 - Varicose veins of lower extremities without ulcer or inflammation" ~ NA,
      Diagnosa_01 == "I840 - Internal thrombosed haemorrhoids" ~ NA,
      Diagnosa_01 == "I880 - Nonspecific mesenteric lymphadenitis" ~ NA,
      Diagnosa_01 == "J00 - Acute nasopharyngitis [common cold]" ~ NA,
      Diagnosa_01 == "J020 - Streptococcal pharyngitis" ~ NA,
      Diagnosa_01 == "J030 - Streptococcal tonsillitis" ~ NA,
      Diagnosa_01 == "J040 - Acute laryngitis" ~ NA,
      Diagnosa_01 == "J100 - Influenza with pneumonia, other influenza virus identified" ~ NA,
      Diagnosa_01 == "J120 - Adenoviral pneumonia" ~ NA,
      Diagnosa_01 == "J300 - Vasomotor rhinitis" ~ NA,
      Diagnosa_01 == "J304 - Allergic rhinitis, unspecified" ~ NA,
      Diagnosa_01 == "J340 - Abscess, furuncle and carbuncle of nose" ~ NA,
      Diagnosa_01 == "J40 - Bronchitis, not specified as acute or chronic" ~ NA,
      Diagnosa_01 == "J459 - Asthma, unspecified" ~ NA,
      Diagnosa_01 == "K210 - Gastro-oesophageal reflux disease with oesophagitis" ~ NA,
      Diagnosa_01 == "K291 - Other acute gastritis" ~ NA,
      Diagnosa_01 == "K297 - Gastritis, unspecified" ~ NA,
      Diagnosa_01 == "K30 - Dyspepsia" ~ NA,
      Diagnosa_01 == "K529 - Noninfective gastroenteritis and colitis, unspecified" ~ NA,
      Diagnosa_01 == "L010 - Impeti [any organism] [any site]" ~ NA,
      Diagnosa_01 == "L020 - Cutaneous abscess, furuncle and carbuncle of face" ~ NA,
      Diagnosa_01 == "L022 - Cutaneous abscess, furuncle and carbuncle of trunk" ~ NA,
      Diagnosa_01 == "L028 - Cutaneous abscess, furuncle and carbuncle of other sites" ~ NA,
      Diagnosa_01 == "L080 - Pyoderma" ~ NA,
      Diagnosa_01 == "L081 - Erythrasma" ~ NA,
      Diagnosa_01 == "L089 - Local infection of skin and subcutaneous tissue, unspecified" ~ NA,
      Diagnosa_01 == "L200 - Besnier's pruri" ~ NA,
      Diagnosa_01 == "L210 - Seborrhoea capitis" ~ NA,
      Diagnosa_01 == "L240 - Irritant contact dermatitis due to detergents" ~ NA,
      Diagnosa_01 == "L270 - Generalized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_01 == "L271 - Localized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_01 == "L272 - Dermatitis due to ingested food" ~ NA,
      Diagnosa_01 == "L300 - Nummular dermatitis" ~ NA,
      Diagnosa_01 == "L309 - Dermatitis, unspecified" ~ NA,
      Diagnosa_01 == "L42 - Pityriasis rosea" ~ NA,
      Diagnosa_01 == "L500 - Allergic urticaria" ~ NA,
      Diagnosa_01 == "L700 - Acne vulgaris" ~ NA,
      Diagnosa_01 == "L710 - Perioral dermatitis" ~ NA,
      Diagnosa_01 == "L732 - Hidradenitis suppurativa" ~ NA,
      Diagnosa_01 == "L743 - Miliaria, unspecified" ~ NA,
      Diagnosa_01 == "N12 - Tubulo-interstitial nephritis, not specified as acute or chronic" ~ NA,
      Diagnosa_01 == "N390 - Urinary tract infection, site not specified" ~ NA,
      Diagnosa_01 == "N47 - Redundant prepuce, phimosis and paraphimosis" ~ NA,
      Diagnosa_01 == "N508 - Other specified disorders of male genital organs" ~ NA,
      Diagnosa_01 == "N61 - Inflammatory disorders of breast" ~ NA,
      Diagnosa_01 == "N645 - Other signs and symptoms in breast" ~ NA,
      Diagnosa_01 == "N700 - Acute salpingitis and oophoritis" ~ NA,
      Diagnosa_01 == "N760 - Acute vaginitis" ~ NA,
      Diagnosa_01 == "N762 - Acute vulvitis" ~ NA,
      Diagnosa_01 == "N768 - Other specified inflammation of vagina and vulva" ~ NA,
      Diagnosa_01 == "O000 - Abdominal pregnancy" ~ NA,
      Diagnosa_01 == "O030 - Spontaneous abortion, incomplete, complicated by genital tract and pelvic infection" ~ NA,
      Diagnosa_01 == "O25 - Malnutrition in pregnancy" ~ NA,
      Diagnosa_01 == "O700 - First degree perineal laceration during delivery" ~ NA,
      Diagnosa_01 == "O701 - Second degree perineal laceration during delivery" ~ NA,
      Diagnosa_01 == "O702 - Third degree perineal laceration during delivery" ~ NA,
      Diagnosa_01 == "O703 - Fourth degree perineal laceration during delivery" ~ NA,
      Diagnosa_01 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_01 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_01 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_01 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_01 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_01 == "O921 - Cracked nipple associated with childbirth" ~ NA,
      Diagnosa_01 == "R040 - Epistaxis" ~ NA,
      Diagnosa_01 == "R560 - Febrile convulsions" ~ NA,
      Diagnosa_01 == "R51 - Headache" ~ NA,
      Diagnosa_01 == "T151 - Foreign body in conjunctival sac" ~ NA,
      Diagnosa_01 == "T171 - Foreign body in nostril" ~ NA,
      Diagnosa_01 == "T301 - Burn of first degree, body region unspecified" ~ NA,
      Diagnosa_01 == "T302 - Burn of second degree, body region unspecified" ~ NA,
      Diagnosa_01 == "T753 - Motion sickness" ~ NA,
      Diagnosa_01 == "T782 - Anaphylactic shock, unspecified" ~ NA,
      Diagnosa_01 == "T812 - Accidental puncture and laceration during a procedure, not elsewhere classified" ~ NA,
      Diagnosa_01 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_01 == "Z370 - Single live birth" ~ NA,
      TRUE ~ Diagnosa_01)) %>%
  select(-Diagnosa_01)

nonspes <- nonspes %>%
  mutate(
    Diagnosa_2 = case_when(
      Diagnosa_02 == "A010 - Typhoid fever" ~ NA,
      Diagnosa_02 == "A059 - Bacterial foodborne intoxication, unspecified" ~ NA,
      Diagnosa_02 == "A060 - Acute amoebic dysentery" ~ NA,
      Diagnosa_02 == "A150 - Tuberculosis of lung, confirmed by sputum microscopy with or without culture" ~ NA,
      Diagnosa_02 == "A184 - Tuberculosis of skin and subcutaneous tissue" ~ NA,
      Diagnosa_02 == "A270 - Leptospirosis icterohaemorrhagica" ~ NA,
      Diagnosa_02 == "A300 - Indeterminate leprosy" ~ NA,
      Diagnosa_02 == "A35 - Other tetanus" ~ NA,
      Diagnosa_02 == "A370 - Whooping cough due to Bordetella pertussis" ~ NA,
      Diagnosa_02 == "A46 - Erysipelas" ~ NA,
      Diagnosa_02 == "A510 - Primary genital syphilis" ~ NA,
      Diagnosa_02 == "A520 - Cardiovascular syphilis" ~ NA,
      Diagnosa_02 == "A540 - nococcal infection of lower genitourinary tract without periurethral or accessory gland abscess" ~ NA,
      Diagnosa_02 == "A90 - Dengue fever [classical dengue]" ~ NA,
      Diagnosa_02 == "A91 - Dengue haemorrhagic fever" ~ NA,
      Diagnosa_02 == "A920 - Chikungunya virus disease" ~ NA,
      Diagnosa_02 == "B001 - Herpesviral vesicular dermatitis" ~ NA,
      Diagnosa_02 == "B009 - Herpesviral infection, unspecified" ~ NA,
      Diagnosa_02 == "B019 - Varicella without complication" ~ NA,
      Diagnosa_02 == "B029 - Zoster without complication" ~ NA,
      Diagnosa_02 == "B059 - Measles without complication" ~ NA,
      Diagnosa_02 == "B07 - Viral warts" ~ NA,
      Diagnosa_02 == "B081 - Molluscum contagiosum" ~ NA,
      Diagnosa_02 == "B150 - Hepatitis A with hepatic coma" ~ NA,
      Diagnosa_02 == "B160 - Acute hepatitis B with delta-agent (coinfection) with hepatic coma" ~ NA,
      Diagnosa_02 == "B170 - Acute delta-(super)infection of hepatitis B carrier" ~ NA,
      Diagnosa_02 == "B171 - Acute hepatitis C" ~ NA,
      Diagnosa_02 == "B172 - Acute hepatitis E" ~ NA,
      Diagnosa_02 == "B24 - Unspecified human immunodeficiency virus [HIV] disease" ~ NA,
      Diagnosa_02 == "B260 - Mumps orchitis" ~ NA,
      Diagnosa_02 == "B350 - Tinea barbae and tinea capitis" ~ NA,
      Diagnosa_02 == "B351 - Tinea unguium" ~ NA,
      Diagnosa_02 == "B352 - Tinea manuum" ~ NA,
      Diagnosa_02 == "B353 - Tinea pedis" ~ NA,
      Diagnosa_02 == "B354 - Tinea corporis" ~ NA,
      Diagnosa_02 == "B356 - Tinea cruris" ~ NA,
      Diagnosa_02 == "B359 - Dermatophytosis, unspecified" ~ NA,
      Diagnosa_02 == "B360 - Pityriasis versicolor" ~ NA,
      Diagnosa_02 == "B500 - Plasmodium falciparum malaria with cerebral complications" ~ NA,
      Diagnosa_02 == "B510 - Plasmodium vivax malaria with rupture of spleen" ~ NA,
      Diagnosa_02 == "B520 - Plasmodium malariae malaria with nephropathy" ~ NA,
      Diagnosa_02 == "B530 - Plasmodium ovale malaria" ~ NA,
      Diagnosa_02 == "B54 - Unspecified malaria" ~ NA,
      Diagnosa_02 == "B650 - Schistosomiasis due to Schistosoma haematobium [urinary schistosomiasis]" ~ NA,
      Diagnosa_02 == "B680 - Von Willebrand disease" ~ NA,
      Diagnosa_02 == "B740 - Filariasis due to Wuchereria bancrofti" ~ NA,
      Diagnosa_02 == "B760 - Ancylostomiasis" ~ NA,
      Diagnosa_02 == "B769 - Hookworm disease, unspecified" ~ NA,
      Diagnosa_02 == "B770 - Ascariasis with intestinal complications" ~ NA,
      Diagnosa_02 == "B780 - Intestinal strongyloidiasis" ~ NA,
      Diagnosa_02 == "B850 - Pediculosis due to Pediculus humanus capitis" ~ NA,
      Diagnosa_02 == "B853 - Phthiriasis" ~ NA,
      Diagnosa_02 == "B86 - Scabies" ~ NA,
      Diagnosa_02 == "D179 - Benign lipomatous neoplasm, unspecified" ~ NA,
      Diagnosa_02 == "D500 - Iron deficiency anaemia secondary to blood loss (chronic)" ~ NA,
      Diagnosa_02 == "D649 - Anaemia, unspecified" ~ NA,
      Diagnosa_02 == "E100 - Insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_02 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_02 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ NA,
      Diagnosa_02 == "E162 - Hypoglycaemia, unspecified" ~ NA,
      Diagnosa_02 == "E46 - Unspecified protein-energy malnutrition" ~ NA,
      Diagnosa_02 == "E569 - Vitamin deficiency, unspecified" ~ NA,
      Diagnosa_02 == "E610 - Copper deficiency" ~ NA,
      Diagnosa_02 == "E660 - Obesity due to excess calories" ~ NA,
      Diagnosa_02 == "E740 - Glycogen storage disease" ~ NA,
      Diagnosa_02 == "E785 - Hyperlipidaemia, unspecified" ~ NA,
      Diagnosa_02 == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease" ~ NA,
      Diagnosa_02 == "F450 - Somatization disorder" ~ NA,
      Diagnosa_02 == "G430 - Migraine without aura [common migraine]" ~ NA,
      Diagnosa_02 == "G442 - Tension-type headache" ~ NA,
      Diagnosa_02 == "G470 - Disorders of initiating and maintaining sleep [insomnias]" ~ NA,
      Diagnosa_02 == "G510 - Bell's palsy" ~ NA,
      Diagnosa_02 == "H000 - Hordeolum and other deep inflammation of eyelid" ~ NA,
      Diagnosa_02 == "H010 - Blepharitis" ~ NA,
      Diagnosa_02 == "H020 - Entropion and trichiasis of eyelid" ~ NA,
      Diagnosa_02 == "H100 - Mucopurulent conjunctivitis" ~ NA,
      Diagnosa_02 == "H113 - Conjunctival haemorrhage" ~ NA,
      Diagnosa_02 == "H151 - Episcleritis" ~ NA,
      Diagnosa_02 == "H162 - Keratoconjunctivitis" ~ NA,
      Diagnosa_02 == "H524 - Presbyopia" ~ NA,
      Diagnosa_02 == "H531 - Subjective visual disturbances" ~ NA,
      Diagnosa_02 == "H600 - Abscess of external ear" ~ NA,
      Diagnosa_02 == "H612 - Impacted cerumen" ~ NA,
      Diagnosa_02 == "H669 - Otitis media, unspecified" ~ NA,
      Diagnosa_02 == "H810 - Meniere's disease" ~ NA,
      Diagnosa_02 == "I10 - Essential (primary) hypertension" ~ NA,
      Diagnosa_02 == "I839 - Varicose veins of lower extremities without ulcer or inflammation" ~ NA,
      Diagnosa_02 == "I840 - Internal thrombosed haemorrhoids" ~ NA,
      Diagnosa_02 == "I880 - Nonspecific mesenteric lymphadenitis" ~ NA,
      Diagnosa_02 == "J00 - Acute nasopharyngitis [common cold]" ~ NA,
      Diagnosa_02 == "J020 - Streptococcal pharyngitis" ~ NA,
      Diagnosa_02 == "J030 - Streptococcal tonsillitis" ~ NA,
      Diagnosa_02 == "J040 - Acute laryngitis" ~ NA,
      Diagnosa_02 == "J100 - Influenza with pneumonia, other influenza virus identified" ~ NA,
      Diagnosa_02 == "J120 - Adenoviral pneumonia" ~ NA,
      Diagnosa_02 == "J300 - Vasomotor rhinitis" ~ NA,
      Diagnosa_02 == "J304 - Allergic rhinitis, unspecified" ~ NA,
      Diagnosa_02 == "J340 - Abscess, furuncle and carbuncle of nose" ~ NA,
      Diagnosa_02 == "J40 - Bronchitis, not specified as acute or chronic" ~ NA,
      Diagnosa_02 == "J459 - Asthma, unspecified" ~ NA,
      Diagnosa_02 == "K210 - Gastro-oesophageal reflux disease with oesophagitis" ~ NA,
      Diagnosa_02 == "K291 - Other acute gastritis" ~ NA,
      Diagnosa_02 == "K297 - Gastritis, unspecified" ~ NA,
      Diagnosa_02 == "K30 - Dyspepsia" ~ NA,
      Diagnosa_02 == "K529 - Noninfective gastroenteritis and colitis, unspecified" ~ NA,
      Diagnosa_02 == "L010 - Impeti [any organism] [any site]" ~ NA,
      Diagnosa_02 == "L020 - Cutaneous abscess, furuncle and carbuncle of face" ~ NA,
      Diagnosa_02 == "L022 - Cutaneous abscess, furuncle and carbuncle of trunk" ~ NA,
      Diagnosa_02 == "L028 - Cutaneous abscess, furuncle and carbuncle of other sites" ~ NA,
      Diagnosa_02 == "L080 - Pyoderma" ~ NA,
      Diagnosa_02 == "L081 - Erythrasma" ~ NA,
      Diagnosa_02 == "L089 - Local infection of skin and subcutaneous tissue, unspecified" ~ NA,
      Diagnosa_02 == "L200 - Besnier's pruri" ~ NA,
      Diagnosa_02 == "L210 - Seborrhoea capitis" ~ NA,
      Diagnosa_02 == "L240 - Irritant contact dermatitis due to detergents" ~ NA,
      Diagnosa_02 == "L270 - Generalized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_02 == "L271 - Localized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_02 == "L272 - Dermatitis due to ingested food" ~ NA,
      Diagnosa_02 == "L300 - Nummular dermatitis" ~ NA,
      Diagnosa_02 == "L309 - Dermatitis, unspecified" ~ NA,
      Diagnosa_02 == "L42 - Pityriasis rosea" ~ NA,
      Diagnosa_02 == "L500 - Allergic urticaria" ~ NA,
      Diagnosa_02 == "L700 - Acne vulgaris" ~ NA,
      Diagnosa_02 == "L710 - Perioral dermatitis" ~ NA,
      Diagnosa_02 == "L732 - Hidradenitis suppurativa" ~ NA,
      Diagnosa_02 == "L743 - Miliaria, unspecified" ~ NA,
      Diagnosa_02 == "N12 - Tubulo-interstitial nephritis, not specified as acute or chronic" ~ NA,
      Diagnosa_02 == "N390 - Urinary tract infection, site not specified" ~ NA,
      Diagnosa_02 == "N47 - Redundant prepuce, phimosis and paraphimosis" ~ NA,
      Diagnosa_02 == "N508 - Other specified disorders of male genital organs" ~ NA,
      Diagnosa_02 == "N61 - Inflammatory disorders of breast" ~ NA,
      Diagnosa_02 == "N645 - Other signs and symptoms in breast" ~ NA,
      Diagnosa_02 == "N700 - Acute salpingitis and oophoritis" ~ NA,
      Diagnosa_02 == "N760 - Acute vaginitis" ~ NA,
      Diagnosa_02 == "N762 - Acute vulvitis" ~ NA,
      Diagnosa_02 == "N768 - Other specified inflammation of vagina and vulva" ~ NA,
      Diagnosa_02 == "O000 - Abdominal pregnancy" ~ NA,
      Diagnosa_02 == "O030 - Spontaneous abortion, incomplete, complicated by genital tract and pelvic infection" ~ NA,
      Diagnosa_02 == "O25 - Malnutrition in pregnancy" ~ NA,
      Diagnosa_02 == "O700 - First degree perineal laceration during delivery" ~ NA,
      Diagnosa_02 == "O701 - Second degree perineal laceration during delivery" ~ NA,
      Diagnosa_02 == "O702 - Third degree perineal laceration during delivery" ~ NA,
      Diagnosa_02 == "O703 - Fourth degree perineal laceration during delivery" ~ NA,
      Diagnosa_02 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_02 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_02 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_02 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_02 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_02 == "O921 - Cracked nipple associated with childbirth" ~ NA,
      Diagnosa_02 == "R040 - Epistaxis" ~ NA,
      Diagnosa_02 == "R560 - Febrile convulsions" ~ NA,
      Diagnosa_02 == "R51 - Headache" ~ NA,
      Diagnosa_02 == "T151 - Foreign body in conjunctival sac" ~ NA,
      Diagnosa_02 == "T171 - Foreign body in nostril" ~ NA,
      Diagnosa_02 == "T301 - Burn of first degree, body region unspecified" ~ NA,
      Diagnosa_02 == "T302 - Burn of second degree, body region unspecified" ~ NA,
      Diagnosa_02 == "T753 - Motion sickness" ~ NA,
      Diagnosa_02 == "T782 - Anaphylactic shock, unspecified" ~ NA,
      Diagnosa_02 == "T812 - Accidental puncture and laceration during a procedure, not elsewhere classified" ~ NA,
      Diagnosa_02 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_02 == "Z370 - Single live birth" ~ NA,
      TRUE ~ Diagnosa_02)) %>%
  select(-Diagnosa_02)

nonspes <- nonspes %>%
  mutate(
    Diagnosa_3 = case_when(
      Diagnosa_03 == "A010 - Typhoid fever" ~ NA,
      Diagnosa_03 == "A059 - Bacterial foodborne intoxication, unspecified" ~ NA,
      Diagnosa_03 == "A060 - Acute amoebic dysentery" ~ NA,
      Diagnosa_03 == "A150 - Tuberculosis of lung, confirmed by sputum microscopy with or without culture" ~ NA,
      Diagnosa_03 == "A184 - Tuberculosis of skin and subcutaneous tissue" ~ NA,
      Diagnosa_03 == "A270 - Leptospirosis icterohaemorrhagica" ~ NA,
      Diagnosa_03 == "A300 - Indeterminate leprosy" ~ NA,
      Diagnosa_03 == "A35 - Other tetanus" ~ NA,
      Diagnosa_03 == "A370 - Whooping cough due to Bordetella pertussis" ~ NA,
      Diagnosa_03 == "A46 - Erysipelas" ~ NA,
      Diagnosa_03 == "A510 - Primary genital syphilis" ~ NA,
      Diagnosa_03 == "A520 - Cardiovascular syphilis" ~ NA,
      Diagnosa_03 == "A540 - nococcal infection of lower genitourinary tract without periurethral or accessory gland abscess" ~ NA,
      Diagnosa_03 == "A90 - Dengue fever [classical dengue]" ~ NA,
      Diagnosa_03 == "A91 - Dengue haemorrhagic fever" ~ NA,
      Diagnosa_03 == "A920 - Chikungunya virus disease" ~ NA,
      Diagnosa_03 == "B001 - Herpesviral vesicular dermatitis" ~ NA,
      Diagnosa_03 == "B009 - Herpesviral infection, unspecified" ~ NA,
      Diagnosa_03 == "B019 - Varicella without complication" ~ NA,
      Diagnosa_03 == "B029 - Zoster without complication" ~ NA,
      Diagnosa_03 == "B059 - Measles without complication" ~ NA,
      Diagnosa_03 == "B07 - Viral warts" ~ NA,
      Diagnosa_03 == "B081 - Molluscum contagiosum" ~ NA,
      Diagnosa_03 == "B150 - Hepatitis A with hepatic coma" ~ NA,
      Diagnosa_03 == "B160 - Acute hepatitis B with delta-agent (coinfection) with hepatic coma" ~ NA,
      Diagnosa_03 == "B170 - Acute delta-(super)infection of hepatitis B carrier" ~ NA,
      Diagnosa_03 == "B171 - Acute hepatitis C" ~ NA,
      Diagnosa_03 == "B172 - Acute hepatitis E" ~ NA,
      Diagnosa_03 == "B24 - Unspecified human immunodeficiency virus [HIV] disease" ~ NA,
      Diagnosa_03 == "B260 - Mumps orchitis" ~ NA,
      Diagnosa_03 == "B350 - Tinea barbae and tinea capitis" ~ NA,
      Diagnosa_03 == "B351 - Tinea unguium" ~ NA,
      Diagnosa_03 == "B352 - Tinea manuum" ~ NA,
      Diagnosa_03 == "B353 - Tinea pedis" ~ NA,
      Diagnosa_03 == "B354 - Tinea corporis" ~ NA,
      Diagnosa_03 == "B356 - Tinea cruris" ~ NA,
      Diagnosa_03 == "B359 - Dermatophytosis, unspecified" ~ NA,
      Diagnosa_03 == "B360 - Pityriasis versicolor" ~ NA,
      Diagnosa_03 == "B500 - Plasmodium falciparum malaria with cerebral complications" ~ NA,
      Diagnosa_03 == "B510 - Plasmodium vivax malaria with rupture of spleen" ~ NA,
      Diagnosa_03 == "B520 - Plasmodium malariae malaria with nephropathy" ~ NA,
      Diagnosa_03 == "B530 - Plasmodium ovale malaria" ~ NA,
      Diagnosa_03 == "B54 - Unspecified malaria" ~ NA,
      Diagnosa_03 == "B650 - Schistosomiasis due to Schistosoma haematobium [urinary schistosomiasis]" ~ NA,
      Diagnosa_03 == "B680 - Von Willebrand disease" ~ NA,
      Diagnosa_03 == "B740 - Filariasis due to Wuchereria bancrofti" ~ NA,
      Diagnosa_03 == "B760 - Ancylostomiasis" ~ NA,
      Diagnosa_03 == "B769 - Hookworm disease, unspecified" ~ NA,
      Diagnosa_03 == "B770 - Ascariasis with intestinal complications" ~ NA,
      Diagnosa_03 == "B780 - Intestinal strongyloidiasis" ~ NA,
      Diagnosa_03 == "B850 - Pediculosis due to Pediculus humanus capitis" ~ NA,
      Diagnosa_03 == "B853 - Phthiriasis" ~ NA,
      Diagnosa_03 == "B86 - Scabies" ~ NA,
      Diagnosa_03 == "D179 - Benign lipomatous neoplasm, unspecified" ~ NA,
      Diagnosa_03 == "D500 - Iron deficiency anaemia secondary to blood loss (chronic)" ~ NA,
      Diagnosa_03 == "D649 - Anaemia, unspecified" ~ NA,
      Diagnosa_03 == "E100 - Insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_03 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_03 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ NA,
      Diagnosa_03 == "E162 - Hypoglycaemia, unspecified" ~ NA,
      Diagnosa_03 == "E46 - Unspecified protein-energy malnutrition" ~ NA,
      Diagnosa_03 == "E569 - Vitamin deficiency, unspecified" ~ NA,
      Diagnosa_03 == "E610 - Copper deficiency" ~ NA,
      Diagnosa_03 == "E660 - Obesity due to excess calories" ~ NA,
      Diagnosa_03 == "E740 - Glycogen storage disease" ~ NA,
      Diagnosa_03 == "E785 - Hyperlipidaemia, unspecified" ~ NA,
      Diagnosa_03 == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease" ~ NA,
      Diagnosa_03 == "F450 - Somatization disorder" ~ NA,
      Diagnosa_03 == "G430 - Migraine without aura [common migraine]" ~ NA,
      Diagnosa_03 == "G442 - Tension-type headache" ~ NA,
      Diagnosa_03 == "G470 - Disorders of initiating and maintaining sleep [insomnias]" ~ NA,
      Diagnosa_03 == "G510 - Bell's palsy" ~ NA,
      Diagnosa_03 == "H000 - Hordeolum and other deep inflammation of eyelid" ~ NA,
      Diagnosa_03 == "H010 - Blepharitis" ~ NA,
      Diagnosa_03 == "H020 - Entropion and trichiasis of eyelid" ~ NA,
      Diagnosa_03 == "H100 - Mucopurulent conjunctivitis" ~ NA,
      Diagnosa_03 == "H113 - Conjunctival haemorrhage" ~ NA,
      Diagnosa_03 == "H151 - Episcleritis" ~ NA,
      Diagnosa_03 == "H162 - Keratoconjunctivitis" ~ NA,
      Diagnosa_03 == "H524 - Presbyopia" ~ NA,
      Diagnosa_03 == "H531 - Subjective visual disturbances" ~ NA,
      Diagnosa_03 == "H600 - Abscess of external ear" ~ NA,
      Diagnosa_03 == "H612 - Impacted cerumen" ~ NA,
      Diagnosa_03 == "H669 - Otitis media, unspecified" ~ NA,
      Diagnosa_03 == "H810 - Meniere's disease" ~ NA,
      Diagnosa_03 == "I10 - Essential (primary) hypertension" ~ NA,
      Diagnosa_03 == "I839 - Varicose veins of lower extremities without ulcer or inflammation" ~ NA,
      Diagnosa_03 == "I840 - Internal thrombosed haemorrhoids" ~ NA,
      Diagnosa_03 == "I880 - Nonspecific mesenteric lymphadenitis" ~ NA,
      Diagnosa_03 == "J00 - Acute nasopharyngitis [common cold]" ~ NA,
      Diagnosa_03 == "J020 - Streptococcal pharyngitis" ~ NA,
      Diagnosa_03 == "J030 - Streptococcal tonsillitis" ~ NA,
      Diagnosa_03 == "J040 - Acute laryngitis" ~ NA,
      Diagnosa_03 == "J100 - Influenza with pneumonia, other influenza virus identified" ~ NA,
      Diagnosa_03 == "J120 - Adenoviral pneumonia" ~ NA,
      Diagnosa_03 == "J300 - Vasomotor rhinitis" ~ NA,
      Diagnosa_03 == "J304 - Allergic rhinitis, unspecified" ~ NA,
      Diagnosa_03 == "J340 - Abscess, furuncle and carbuncle of nose" ~ NA,
      Diagnosa_03 == "J40 - Bronchitis, not specified as acute or chronic" ~ NA,
      Diagnosa_03 == "J459 - Asthma, unspecified" ~ NA,
      Diagnosa_03 == "K210 - Gastro-oesophageal reflux disease with oesophagitis" ~ NA,
      Diagnosa_03 == "K291 - Other acute gastritis" ~ NA,
      Diagnosa_03 == "K297 - Gastritis, unspecified" ~ NA,
      Diagnosa_03 == "K30 - Dyspepsia" ~ NA,
      Diagnosa_03 == "K529 - Noninfective gastroenteritis and colitis, unspecified" ~ NA,
      Diagnosa_03 == "L010 - Impeti [any organism] [any site]" ~ NA,
      Diagnosa_03 == "L020 - Cutaneous abscess, furuncle and carbuncle of face" ~ NA,
      Diagnosa_03 == "L022 - Cutaneous abscess, furuncle and carbuncle of trunk" ~ NA,
      Diagnosa_03 == "L028 - Cutaneous abscess, furuncle and carbuncle of other sites" ~ NA,
      Diagnosa_03 == "L080 - Pyoderma" ~ NA,
      Diagnosa_03 == "L081 - Erythrasma" ~ NA,
      Diagnosa_03 == "L089 - Local infection of skin and subcutaneous tissue, unspecified" ~ NA,
      Diagnosa_03 == "L200 - Besnier's pruri" ~ NA,
      Diagnosa_03 == "L210 - Seborrhoea capitis" ~ NA,
      Diagnosa_03 == "L240 - Irritant contact dermatitis due to detergents" ~ NA,
      Diagnosa_03 == "L270 - Generalized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_03 == "L271 - Localized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_03 == "L272 - Dermatitis due to ingested food" ~ NA,
      Diagnosa_03 == "L300 - Nummular dermatitis" ~ NA,
      Diagnosa_03 == "L309 - Dermatitis, unspecified" ~ NA,
      Diagnosa_03 == "L42 - Pityriasis rosea" ~ NA,
      Diagnosa_03 == "L500 - Allergic urticaria" ~ NA,
      Diagnosa_03 == "L700 - Acne vulgaris" ~ NA,
      Diagnosa_03 == "L710 - Perioral dermatitis" ~ NA,
      Diagnosa_03 == "L732 - Hidradenitis suppurativa" ~ NA,
      Diagnosa_03 == "L743 - Miliaria, unspecified" ~ NA,
      Diagnosa_03 == "N12 - Tubulo-interstitial nephritis, not specified as acute or chronic" ~ NA,
      Diagnosa_03 == "N390 - Urinary tract infection, site not specified" ~ NA,
      Diagnosa_03 == "N47 - Redundant prepuce, phimosis and paraphimosis" ~ NA,
      Diagnosa_03 == "N508 - Other specified disorders of male genital organs" ~ NA,
      Diagnosa_03 == "N61 - Inflammatory disorders of breast" ~ NA,
      Diagnosa_03 == "N645 - Other signs and symptoms in breast" ~ NA,
      Diagnosa_03 == "N700 - Acute salpingitis and oophoritis" ~ NA,
      Diagnosa_03 == "N760 - Acute vaginitis" ~ NA,
      Diagnosa_03 == "N762 - Acute vulvitis" ~ NA,
      Diagnosa_03 == "N768 - Other specified inflammation of vagina and vulva" ~ NA,
      Diagnosa_03 == "O000 - Abdominal pregnancy" ~ NA,
      Diagnosa_03 == "O030 - Spontaneous abortion, incomplete, complicated by genital tract and pelvic infection" ~ NA,
      Diagnosa_03 == "O25 - Malnutrition in pregnancy" ~ NA,
      Diagnosa_03 == "O700 - First degree perineal laceration during delivery" ~ NA,
      Diagnosa_03 == "O701 - Second degree perineal laceration during delivery" ~ NA,
      Diagnosa_03 == "O702 - Third degree perineal laceration during delivery" ~ NA,
      Diagnosa_03 == "O703 - Fourth degree perineal laceration during delivery" ~ NA,
      Diagnosa_03 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_03 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_03 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_03 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_03 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_03 == "O921 - Cracked nipple associated with childbirth" ~ NA,
      Diagnosa_03 == "R040 - Epistaxis" ~ NA,
      Diagnosa_03 == "R560 - Febrile convulsions" ~ NA,
      Diagnosa_03 == "R51 - Headache" ~ NA,
      Diagnosa_03 == "T151 - Foreign body in conjunctival sac" ~ NA,
      Diagnosa_03 == "T171 - Foreign body in nostril" ~ NA,
      Diagnosa_03 == "T301 - Burn of first degree, body region unspecified" ~ NA,
      Diagnosa_03 == "T302 - Burn of second degree, body region unspecified" ~ NA,
      Diagnosa_03 == "T753 - Motion sickness" ~ NA,
      Diagnosa_03 == "T782 - Anaphylactic shock, unspecified" ~ NA,
      Diagnosa_03 == "T812 - Accidental puncture and laceration during a procedure, not elsewhere classified" ~ NA,
      Diagnosa_03 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_03 == "Z370 - Single live birth" ~ NA,
      TRUE ~ Diagnosa_03)) %>%
  select(-Diagnosa_03)

nonspes <- nonspes %>%
  mutate(
    Diagnosa_4 = case_when(
      Diagnosa_04 == "A010 - Typhoid fever" ~ NA,
      Diagnosa_04 == "A059 - Bacterial foodborne intoxication, unspecified" ~ NA,
      Diagnosa_04 == "A060 - Acute amoebic dysentery" ~ NA,
      Diagnosa_04 == "A150 - Tuberculosis of lung, confirmed by sputum microscopy with or without culture" ~ NA,
      Diagnosa_04 == "A184 - Tuberculosis of skin and subcutaneous tissue" ~ NA,
      Diagnosa_04 == "A270 - Leptospirosis icterohaemorrhagica" ~ NA,
      Diagnosa_04 == "A300 - Indeterminate leprosy" ~ NA,
      Diagnosa_04 == "A35 - Other tetanus" ~ NA,
      Diagnosa_04 == "A370 - Whooping cough due to Bordetella pertussis" ~ NA,
      Diagnosa_04 == "A46 - Erysipelas" ~ NA,
      Diagnosa_04 == "A510 - Primary genital syphilis" ~ NA,
      Diagnosa_04 == "A520 - Cardiovascular syphilis" ~ NA,
      Diagnosa_04 == "A540 - nococcal infection of lower genitourinary tract without periurethral or accessory gland abscess" ~ NA,
      Diagnosa_04 == "A90 - Dengue fever [classical dengue]" ~ NA,
      Diagnosa_04 == "A91 - Dengue haemorrhagic fever" ~ NA,
      Diagnosa_04 == "A920 - Chikungunya virus disease" ~ NA,
      Diagnosa_04 == "B001 - Herpesviral vesicular dermatitis" ~ NA,
      Diagnosa_04 == "B009 - Herpesviral infection, unspecified" ~ NA,
      Diagnosa_04 == "B019 - Varicella without complication" ~ NA,
      Diagnosa_04 == "B029 - Zoster without complication" ~ NA,
      Diagnosa_04 == "B059 - Measles without complication" ~ NA,
      Diagnosa_04 == "B07 - Viral warts" ~ NA,
      Diagnosa_04 == "B081 - Molluscum contagiosum" ~ NA,
      Diagnosa_04 == "B150 - Hepatitis A with hepatic coma" ~ NA,
      Diagnosa_04 == "B160 - Acute hepatitis B with delta-agent (coinfection) with hepatic coma" ~ NA,
      Diagnosa_04 == "B170 - Acute delta-(super)infection of hepatitis B carrier" ~ NA,
      Diagnosa_04 == "B171 - Acute hepatitis C" ~ NA,
      Diagnosa_04 == "B172 - Acute hepatitis E" ~ NA,
      Diagnosa_04 == "B24 - Unspecified human immunodeficiency virus [HIV] disease" ~ NA,
      Diagnosa_04 == "B260 - Mumps orchitis" ~ NA,
      Diagnosa_04 == "B350 - Tinea barbae and tinea capitis" ~ NA,
      Diagnosa_04 == "B351 - Tinea unguium" ~ NA,
      Diagnosa_04 == "B352 - Tinea manuum" ~ NA,
      Diagnosa_04 == "B353 - Tinea pedis" ~ NA,
      Diagnosa_04 == "B354 - Tinea corporis" ~ NA,
      Diagnosa_04 == "B356 - Tinea cruris" ~ NA,
      Diagnosa_04 == "B359 - Dermatophytosis, unspecified" ~ NA,
      Diagnosa_04 == "B360 - Pityriasis versicolor" ~ NA,
      Diagnosa_04 == "B500 - Plasmodium falciparum malaria with cerebral complications" ~ NA,
      Diagnosa_04 == "B510 - Plasmodium vivax malaria with rupture of spleen" ~ NA,
      Diagnosa_04 == "B520 - Plasmodium malariae malaria with nephropathy" ~ NA,
      Diagnosa_04 == "B530 - Plasmodium ovale malaria" ~ NA,
      Diagnosa_04 == "B54 - Unspecified malaria" ~ NA,
      Diagnosa_04 == "B650 - Schistosomiasis due to Schistosoma haematobium [urinary schistosomiasis]" ~ NA,
      Diagnosa_04 == "B680 - Von Willebrand disease" ~ NA,
      Diagnosa_04 == "B740 - Filariasis due to Wuchereria bancrofti" ~ NA,
      Diagnosa_04 == "B760 - Ancylostomiasis" ~ NA,
      Diagnosa_04 == "B769 - Hookworm disease, unspecified" ~ NA,
      Diagnosa_04 == "B770 - Ascariasis with intestinal complications" ~ NA,
      Diagnosa_04 == "B780 - Intestinal strongyloidiasis" ~ NA,
      Diagnosa_04 == "B850 - Pediculosis due to Pediculus humanus capitis" ~ NA,
      Diagnosa_04 == "B853 - Phthiriasis" ~ NA,
      Diagnosa_04 == "B86 - Scabies" ~ NA,
      Diagnosa_04 == "D179 - Benign lipomatous neoplasm, unspecified" ~ NA,
      Diagnosa_04 == "D500 - Iron deficiency anaemia secondary to blood loss (chronic)" ~ NA,
      Diagnosa_04 == "D649 - Anaemia, unspecified" ~ NA,
      Diagnosa_04 == "E100 - Insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_04 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_04 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ NA,
      Diagnosa_04 == "E162 - Hypoglycaemia, unspecified" ~ NA,
      Diagnosa_04 == "E46 - Unspecified protein-energy malnutrition" ~ NA,
      Diagnosa_04 == "E569 - Vitamin deficiency, unspecified" ~ NA,
      Diagnosa_04 == "E610 - Copper deficiency" ~ NA,
      Diagnosa_04 == "E660 - Obesity due to excess calories" ~ NA,
      Diagnosa_04 == "E740 - Glycogen storage disease" ~ NA,
      Diagnosa_04 == "E785 - Hyperlipidaemia, unspecified" ~ NA,
      Diagnosa_04 == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease" ~ NA,
      Diagnosa_04 == "F450 - Somatization disorder" ~ NA,
      Diagnosa_04 == "G430 - Migraine without aura [common migraine]" ~ NA,
      Diagnosa_04 == "G442 - Tension-type headache" ~ NA,
      Diagnosa_04 == "G470 - Disorders of initiating and maintaining sleep [insomnias]" ~ NA,
      Diagnosa_04 == "G510 - Bell's palsy" ~ NA,
      Diagnosa_04 == "H000 - Hordeolum and other deep inflammation of eyelid" ~ NA,
      Diagnosa_04 == "H010 - Blepharitis" ~ NA,
      Diagnosa_04 == "H020 - Entropion and trichiasis of eyelid" ~ NA,
      Diagnosa_04 == "H100 - Mucopurulent conjunctivitis" ~ NA,
      Diagnosa_04 == "H113 - Conjunctival haemorrhage" ~ NA,
      Diagnosa_04 == "H151 - Episcleritis" ~ NA,
      Diagnosa_04 == "H162 - Keratoconjunctivitis" ~ NA,
      Diagnosa_04 == "H524 - Presbyopia" ~ NA,
      Diagnosa_04 == "H531 - Subjective visual disturbances" ~ NA,
      Diagnosa_04 == "H600 - Abscess of external ear" ~ NA,
      Diagnosa_04 == "H612 - Impacted cerumen" ~ NA,
      Diagnosa_04 == "H669 - Otitis media, unspecified" ~ NA,
      Diagnosa_04 == "H810 - Meniere's disease" ~ NA,
      Diagnosa_04 == "I10 - Essential (primary) hypertension" ~ NA,
      Diagnosa_04 == "I839 - Varicose veins of lower extremities without ulcer or inflammation" ~ NA,
      Diagnosa_04 == "I840 - Internal thrombosed haemorrhoids" ~ NA,
      Diagnosa_04 == "I880 - Nonspecific mesenteric lymphadenitis" ~ NA,
      Diagnosa_04 == "J00 - Acute nasopharyngitis [common cold]" ~ NA,
      Diagnosa_04 == "J020 - Streptococcal pharyngitis" ~ NA,
      Diagnosa_04 == "J030 - Streptococcal tonsillitis" ~ NA,
      Diagnosa_04 == "J040 - Acute laryngitis" ~ NA,
      Diagnosa_04 == "J100 - Influenza with pneumonia, other influenza virus identified" ~ NA,
      Diagnosa_04 == "J120 - Adenoviral pneumonia" ~ NA,
      Diagnosa_04 == "J300 - Vasomotor rhinitis" ~ NA,
      Diagnosa_04 == "J304 - Allergic rhinitis, unspecified" ~ NA,
      Diagnosa_04 == "J340 - Abscess, furuncle and carbuncle of nose" ~ NA,
      Diagnosa_04 == "J40 - Bronchitis, not specified as acute or chronic" ~ NA,
      Diagnosa_04 == "J459 - Asthma, unspecified" ~ NA,
      Diagnosa_04 == "K210 - Gastro-oesophageal reflux disease with oesophagitis" ~ NA,
      Diagnosa_04 == "K291 - Other acute gastritis" ~ NA,
      Diagnosa_04 == "K297 - Gastritis, unspecified" ~ NA,
      Diagnosa_04 == "K30 - Dyspepsia" ~ NA,
      Diagnosa_04 == "K529 - Noninfective gastroenteritis and colitis, unspecified" ~ NA,
      Diagnosa_04 == "L010 - Impeti [any organism] [any site]" ~ NA,
      Diagnosa_04 == "L020 - Cutaneous abscess, furuncle and carbuncle of face" ~ NA,
      Diagnosa_04 == "L022 - Cutaneous abscess, furuncle and carbuncle of trunk" ~ NA,
      Diagnosa_04 == "L028 - Cutaneous abscess, furuncle and carbuncle of other sites" ~ NA,
      Diagnosa_04 == "L080 - Pyoderma" ~ NA,
      Diagnosa_04 == "L081 - Erythrasma" ~ NA,
      Diagnosa_04 == "L089 - Local infection of skin and subcutaneous tissue, unspecified" ~ NA,
      Diagnosa_04 == "L200 - Besnier's pruri" ~ NA,
      Diagnosa_04 == "L210 - Seborrhoea capitis" ~ NA,
      Diagnosa_04 == "L240 - Irritant contact dermatitis due to detergents" ~ NA,
      Diagnosa_04 == "L270 - Generalized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_04 == "L271 - Localized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_04 == "L272 - Dermatitis due to ingested food" ~ NA,
      Diagnosa_04 == "L300 - Nummular dermatitis" ~ NA,
      Diagnosa_04 == "L309 - Dermatitis, unspecified" ~ NA,
      Diagnosa_04 == "L42 - Pityriasis rosea" ~ NA,
      Diagnosa_04 == "L500 - Allergic urticaria" ~ NA,
      Diagnosa_04 == "L700 - Acne vulgaris" ~ NA,
      Diagnosa_04 == "L710 - Perioral dermatitis" ~ NA,
      Diagnosa_04 == "L732 - Hidradenitis suppurativa" ~ NA,
      Diagnosa_04 == "L743 - Miliaria, unspecified" ~ NA,
      Diagnosa_04 == "N12 - Tubulo-interstitial nephritis, not specified as acute or chronic" ~ NA,
      Diagnosa_04 == "N390 - Urinary tract infection, site not specified" ~ NA,
      Diagnosa_04 == "N47 - Redundant prepuce, phimosis and paraphimosis" ~ NA,
      Diagnosa_04 == "N508 - Other specified disorders of male genital organs" ~ NA,
      Diagnosa_04 == "N61 - Inflammatory disorders of breast" ~ NA,
      Diagnosa_04 == "N645 - Other signs and symptoms in breast" ~ NA,
      Diagnosa_04 == "N700 - Acute salpingitis and oophoritis" ~ NA,
      Diagnosa_04 == "N760 - Acute vaginitis" ~ NA,
      Diagnosa_04 == "N762 - Acute vulvitis" ~ NA,
      Diagnosa_04 == "N768 - Other specified inflammation of vagina and vulva" ~ NA,
      Diagnosa_04 == "O000 - Abdominal pregnancy" ~ NA,
      Diagnosa_04 == "O030 - Spontaneous abortion, incomplete, complicated by genital tract and pelvic infection" ~ NA,
      Diagnosa_04 == "O25 - Malnutrition in pregnancy" ~ NA,
      Diagnosa_04 == "O700 - First degree perineal laceration during delivery" ~ NA,
      Diagnosa_04 == "O701 - Second degree perineal laceration during delivery" ~ NA,
      Diagnosa_04 == "O702 - Third degree perineal laceration during delivery" ~ NA,
      Diagnosa_04 == "O703 - Fourth degree perineal laceration during delivery" ~ NA,
      Diagnosa_04 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_04 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_04 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_04 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_04 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_04 == "O921 - Cracked nipple associated with childbirth" ~ NA,
      Diagnosa_04 == "R040 - Epistaxis" ~ NA,
      Diagnosa_04 == "R560 - Febrile convulsions" ~ NA,
      Diagnosa_04 == "R51 - Headache" ~ NA,
      Diagnosa_04 == "T151 - Foreign body in conjunctival sac" ~ NA,
      Diagnosa_04 == "T171 - Foreign body in nostril" ~ NA,
      Diagnosa_04 == "T301 - Burn of first degree, body region unspecified" ~ NA,
      Diagnosa_04 == "T302 - Burn of second degree, body region unspecified" ~ NA,
      Diagnosa_04 == "T753 - Motion sickness" ~ NA,
      Diagnosa_04 == "T782 - Anaphylactic shock, unspecified" ~ NA,
      Diagnosa_04 == "T812 - Accidental puncture and laceration during a procedure, not elsewhere classified" ~ NA,
      Diagnosa_04 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_04 == "Z370 - Single live birth" ~ NA,
      TRUE ~ Diagnosa_04)) %>%
  select(-Diagnosa_04)

nonspes <- nonspes %>%
  mutate(
    Diagnosa_5 = case_when(
      Diagnosa_05 == "A010 - Typhoid fever" ~ NA,
      Diagnosa_05 == "A059 - Bacterial foodborne intoxication, unspecified" ~ NA,
      Diagnosa_05 == "A060 - Acute amoebic dysentery" ~ NA,
      Diagnosa_05 == "A150 - Tuberculosis of lung, confirmed by sputum microscopy with or without culture" ~ NA,
      Diagnosa_05 == "A184 - Tuberculosis of skin and subcutaneous tissue" ~ NA,
      Diagnosa_05 == "A270 - Leptospirosis icterohaemorrhagica" ~ NA,
      Diagnosa_05 == "A300 - Indeterminate leprosy" ~ NA,
      Diagnosa_05 == "A35 - Other tetanus" ~ NA,
      Diagnosa_05 == "A370 - Whooping cough due to Bordetella pertussis" ~ NA,
      Diagnosa_05 == "A46 - Erysipelas" ~ NA,
      Diagnosa_05 == "A510 - Primary genital syphilis" ~ NA,
      Diagnosa_05 == "A520 - Cardiovascular syphilis" ~ NA,
      Diagnosa_05 == "A540 - nococcal infection of lower genitourinary tract without periurethral or accessory gland abscess" ~ NA,
      Diagnosa_05 == "A90 - Dengue fever [classical dengue]" ~ NA,
      Diagnosa_05 == "A91 - Dengue haemorrhagic fever" ~ NA,
      Diagnosa_05 == "A920 - Chikungunya virus disease" ~ NA,
      Diagnosa_05 == "B001 - Herpesviral vesicular dermatitis" ~ NA,
      Diagnosa_05 == "B009 - Herpesviral infection, unspecified" ~ NA,
      Diagnosa_05 == "B019 - Varicella without complication" ~ NA,
      Diagnosa_05 == "B029 - Zoster without complication" ~ NA,
      Diagnosa_05 == "B059 - Measles without complication" ~ NA,
      Diagnosa_05 == "B07 - Viral warts" ~ NA,
      Diagnosa_05 == "B081 - Molluscum contagiosum" ~ NA,
      Diagnosa_05 == "B150 - Hepatitis A with hepatic coma" ~ NA,
      Diagnosa_05 == "B160 - Acute hepatitis B with delta-agent (coinfection) with hepatic coma" ~ NA,
      Diagnosa_05 == "B170 - Acute delta-(super)infection of hepatitis B carrier" ~ NA,
      Diagnosa_05 == "B171 - Acute hepatitis C" ~ NA,
      Diagnosa_05 == "B172 - Acute hepatitis E" ~ NA,
      Diagnosa_05 == "B24 - Unspecified human immunodeficiency virus [HIV] disease" ~ NA,
      Diagnosa_05 == "B260 - Mumps orchitis" ~ NA,
      Diagnosa_05 == "B350 - Tinea barbae and tinea capitis" ~ NA,
      Diagnosa_05 == "B351 - Tinea unguium" ~ NA,
      Diagnosa_05 == "B352 - Tinea manuum" ~ NA,
      Diagnosa_05 == "B353 - Tinea pedis" ~ NA,
      Diagnosa_05 == "B354 - Tinea corporis" ~ NA,
      Diagnosa_05 == "B356 - Tinea cruris" ~ NA,
      Diagnosa_05 == "B359 - Dermatophytosis, unspecified" ~ NA,
      Diagnosa_05 == "B360 - Pityriasis versicolor" ~ NA,
      Diagnosa_05 == "B500 - Plasmodium falciparum malaria with cerebral complications" ~ NA,
      Diagnosa_05 == "B510 - Plasmodium vivax malaria with rupture of spleen" ~ NA,
      Diagnosa_05 == "B520 - Plasmodium malariae malaria with nephropathy" ~ NA,
      Diagnosa_05 == "B530 - Plasmodium ovale malaria" ~ NA,
      Diagnosa_05 == "B54 - Unspecified malaria" ~ NA,
      Diagnosa_05 == "B650 - Schistosomiasis due to Schistosoma haematobium [urinary schistosomiasis]" ~ NA,
      Diagnosa_05 == "B680 - Von Willebrand disease" ~ NA,
      Diagnosa_05 == "B740 - Filariasis due to Wuchereria bancrofti" ~ NA,
      Diagnosa_05 == "B760 - Ancylostomiasis" ~ NA,
      Diagnosa_05 == "B769 - Hookworm disease, unspecified" ~ NA,
      Diagnosa_05 == "B770 - Ascariasis with intestinal complications" ~ NA,
      Diagnosa_05 == "B780 - Intestinal strongyloidiasis" ~ NA,
      Diagnosa_05 == "B850 - Pediculosis due to Pediculus humanus capitis" ~ NA,
      Diagnosa_05 == "B853 - Phthiriasis" ~ NA,
      Diagnosa_05 == "B86 - Scabies" ~ NA,
      Diagnosa_05 == "D179 - Benign lipomatous neoplasm, unspecified" ~ NA,
      Diagnosa_05 == "D500 - Iron deficiency anaemia secondary to blood loss (chronic)" ~ NA,
      Diagnosa_05 == "D649 - Anaemia, unspecified" ~ NA,
      Diagnosa_05 == "E100 - Insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_05 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_05 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ NA,
      Diagnosa_05 == "E162 - Hypoglycaemia, unspecified" ~ NA,
      Diagnosa_05 == "E46 - Unspecified protein-energy malnutrition" ~ NA,
      Diagnosa_05 == "E569 - Vitamin deficiency, unspecified" ~ NA,
      Diagnosa_05 == "E610 - Copper deficiency" ~ NA,
      Diagnosa_05 == "E660 - Obesity due to excess calories" ~ NA,
      Diagnosa_05 == "E740 - Glycogen storage disease" ~ NA,
      Diagnosa_05 == "E785 - Hyperlipidaemia, unspecified" ~ NA,
      Diagnosa_05 == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease" ~ NA,
      Diagnosa_05 == "F450 - Somatization disorder" ~ NA,
      Diagnosa_05 == "G430 - Migraine without aura [common migraine]" ~ NA,
      Diagnosa_05 == "G442 - Tension-type headache" ~ NA,
      Diagnosa_05 == "G470 - Disorders of initiating and maintaining sleep [insomnias]" ~ NA,
      Diagnosa_05 == "G510 - Bell's palsy" ~ NA,
      Diagnosa_05 == "H000 - Hordeolum and other deep inflammation of eyelid" ~ NA,
      Diagnosa_05 == "H010 - Blepharitis" ~ NA,
      Diagnosa_05 == "H020 - Entropion and trichiasis of eyelid" ~ NA,
      Diagnosa_05 == "H100 - Mucopurulent conjunctivitis" ~ NA,
      Diagnosa_05 == "H113 - Conjunctival haemorrhage" ~ NA,
      Diagnosa_05 == "H151 - Episcleritis" ~ NA,
      Diagnosa_05 == "H162 - Keratoconjunctivitis" ~ NA,
      Diagnosa_05 == "H524 - Presbyopia" ~ NA,
      Diagnosa_05 == "H531 - Subjective visual disturbances" ~ NA,
      Diagnosa_05 == "H600 - Abscess of external ear" ~ NA,
      Diagnosa_05 == "H612 - Impacted cerumen" ~ NA,
      Diagnosa_05 == "H669 - Otitis media, unspecified" ~ NA,
      Diagnosa_05 == "H810 - Meniere's disease" ~ NA,
      Diagnosa_05 == "I10 - Essential (primary) hypertension" ~ NA,
      Diagnosa_05 == "I839 - Varicose veins of lower extremities without ulcer or inflammation" ~ NA,
      Diagnosa_05 == "I840 - Internal thrombosed haemorrhoids" ~ NA,
      Diagnosa_05 == "I880 - Nonspecific mesenteric lymphadenitis" ~ NA,
      Diagnosa_05 == "J00 - Acute nasopharyngitis [common cold]" ~ NA,
      Diagnosa_05 == "J020 - Streptococcal pharyngitis" ~ NA,
      Diagnosa_05 == "J030 - Streptococcal tonsillitis" ~ NA,
      Diagnosa_05 == "J040 - Acute laryngitis" ~ NA,
      Diagnosa_05 == "J100 - Influenza with pneumonia, other influenza virus identified" ~ NA,
      Diagnosa_05 == "J120 - Adenoviral pneumonia" ~ NA,
      Diagnosa_05 == "J300 - Vasomotor rhinitis" ~ NA,
      Diagnosa_05 == "J304 - Allergic rhinitis, unspecified" ~ NA,
      Diagnosa_05 == "J340 - Abscess, furuncle and carbuncle of nose" ~ NA,
      Diagnosa_05 == "J40 - Bronchitis, not specified as acute or chronic" ~ NA,
      Diagnosa_05 == "J459 - Asthma, unspecified" ~ NA,
      Diagnosa_05 == "K210 - Gastro-oesophageal reflux disease with oesophagitis" ~ NA,
      Diagnosa_05 == "K291 - Other acute gastritis" ~ NA,
      Diagnosa_05 == "K297 - Gastritis, unspecified" ~ NA,
      Diagnosa_05 == "K30 - Dyspepsia" ~ NA,
      Diagnosa_05 == "K529 - Noninfective gastroenteritis and colitis, unspecified" ~ NA,
      Diagnosa_05 == "L010 - Impeti [any organism] [any site]" ~ NA,
      Diagnosa_05 == "L020 - Cutaneous abscess, furuncle and carbuncle of face" ~ NA,
      Diagnosa_05 == "L022 - Cutaneous abscess, furuncle and carbuncle of trunk" ~ NA,
      Diagnosa_05 == "L028 - Cutaneous abscess, furuncle and carbuncle of other sites" ~ NA,
      Diagnosa_05 == "L080 - Pyoderma" ~ NA,
      Diagnosa_05 == "L081 - Erythrasma" ~ NA,
      Diagnosa_05 == "L089 - Local infection of skin and subcutaneous tissue, unspecified" ~ NA,
      Diagnosa_05 == "L200 - Besnier's pruri" ~ NA,
      Diagnosa_05 == "L210 - Seborrhoea capitis" ~ NA,
      Diagnosa_05 == "L240 - Irritant contact dermatitis due to detergents" ~ NA,
      Diagnosa_05 == "L270 - Generalized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_05 == "L271 - Localized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_05 == "L272 - Dermatitis due to ingested food" ~ NA,
      Diagnosa_05 == "L300 - Nummular dermatitis" ~ NA,
      Diagnosa_05 == "L309 - Dermatitis, unspecified" ~ NA,
      Diagnosa_05 == "L42 - Pityriasis rosea" ~ NA,
      Diagnosa_05 == "L500 - Allergic urticaria" ~ NA,
      Diagnosa_05 == "L700 - Acne vulgaris" ~ NA,
      Diagnosa_05 == "L710 - Perioral dermatitis" ~ NA,
      Diagnosa_05 == "L732 - Hidradenitis suppurativa" ~ NA,
      Diagnosa_05 == "L743 - Miliaria, unspecified" ~ NA,
      Diagnosa_05 == "N12 - Tubulo-interstitial nephritis, not specified as acute or chronic" ~ NA,
      Diagnosa_05 == "N390 - Urinary tract infection, site not specified" ~ NA,
      Diagnosa_05 == "N47 - Redundant prepuce, phimosis and paraphimosis" ~ NA,
      Diagnosa_05 == "N508 - Other specified disorders of male genital organs" ~ NA,
      Diagnosa_05 == "N61 - Inflammatory disorders of breast" ~ NA,
      Diagnosa_05 == "N645 - Other signs and symptoms in breast" ~ NA,
      Diagnosa_05 == "N700 - Acute salpingitis and oophoritis" ~ NA,
      Diagnosa_05 == "N760 - Acute vaginitis" ~ NA,
      Diagnosa_05 == "N762 - Acute vulvitis" ~ NA,
      Diagnosa_05 == "N768 - Other specified inflammation of vagina and vulva" ~ NA,
      Diagnosa_05 == "O000 - Abdominal pregnancy" ~ NA,
      Diagnosa_05 == "O030 - Spontaneous abortion, incomplete, complicated by genital tract and pelvic infection" ~ NA,
      Diagnosa_05 == "O25 - Malnutrition in pregnancy" ~ NA,
      Diagnosa_05 == "O700 - First degree perineal laceration during delivery" ~ NA,
      Diagnosa_05 == "O701 - Second degree perineal laceration during delivery" ~ NA,
      Diagnosa_05 == "O702 - Third degree perineal laceration during delivery" ~ NA,
      Diagnosa_05 == "O703 - Fourth degree perineal laceration during delivery" ~ NA,
      Diagnosa_05 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_05 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_05 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_05 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_05 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_05 == "O921 - Cracked nipple associated with childbirth" ~ NA,
      Diagnosa_05 == "R040 - Epistaxis" ~ NA,
      Diagnosa_05 == "R560 - Febrile convulsions" ~ NA,
      Diagnosa_05 == "R51 - Headache" ~ NA,
      Diagnosa_05 == "T151 - Foreign body in conjunctival sac" ~ NA,
      Diagnosa_05 == "T171 - Foreign body in nostril" ~ NA,
      Diagnosa_05 == "T301 - Burn of first degree, body region unspecified" ~ NA,
      Diagnosa_05 == "T302 - Burn of second degree, body region unspecified" ~ NA,
      Diagnosa_05 == "T753 - Motion sickness" ~ NA,
      Diagnosa_05 == "T782 - Anaphylactic shock, unspecified" ~ NA,
      Diagnosa_05 == "T812 - Accidental puncture and laceration during a procedure, not elsewhere classified" ~ NA,
      Diagnosa_05 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_05 == "Z370 - Single live birth" ~ NA,
      TRUE ~ Diagnosa_05)) %>%
  select(-Diagnosa_05)

nonspes <- nonspes %>%
  mutate(
    Diagnosa_6 = case_when(
      Diagnosa_06 == "A010 - Typhoid fever" ~ NA,
      Diagnosa_06 == "A059 - Bacterial foodborne intoxication, unspecified" ~ NA,
      Diagnosa_06 == "A060 - Acute amoebic dysentery" ~ NA,
      Diagnosa_06 == "A150 - Tuberculosis of lung, confirmed by sputum microscopy with or without culture" ~ NA,
      Diagnosa_06 == "A184 - Tuberculosis of skin and subcutaneous tissue" ~ NA,
      Diagnosa_06 == "A270 - Leptospirosis icterohaemorrhagica" ~ NA,
      Diagnosa_06 == "A300 - Indeterminate leprosy" ~ NA,
      Diagnosa_06 == "A35 - Other tetanus" ~ NA,
      Diagnosa_06 == "A370 - Whooping cough due to Bordetella pertussis" ~ NA,
      Diagnosa_06 == "A46 - Erysipelas" ~ NA,
      Diagnosa_06 == "A510 - Primary genital syphilis" ~ NA,
      Diagnosa_06 == "A520 - Cardiovascular syphilis" ~ NA,
      Diagnosa_06 == "A540 - nococcal infection of lower genitourinary tract without periurethral or accessory gland abscess" ~ NA,
      Diagnosa_06 == "A90 - Dengue fever [classical dengue]" ~ NA,
      Diagnosa_06 == "A91 - Dengue haemorrhagic fever" ~ NA,
      Diagnosa_06 == "A920 - Chikungunya virus disease" ~ NA,
      Diagnosa_06 == "B001 - Herpesviral vesicular dermatitis" ~ NA,
      Diagnosa_06 == "B009 - Herpesviral infection, unspecified" ~ NA,
      Diagnosa_06 == "B019 - Varicella without complication" ~ NA,
      Diagnosa_06 == "B029 - Zoster without complication" ~ NA,
      Diagnosa_06 == "B059 - Measles without complication" ~ NA,
      Diagnosa_06 == "B07 - Viral warts" ~ NA,
      Diagnosa_06 == "B081 - Molluscum contagiosum" ~ NA,
      Diagnosa_06 == "B150 - Hepatitis A with hepatic coma" ~ NA,
      Diagnosa_06 == "B160 - Acute hepatitis B with delta-agent (coinfection) with hepatic coma" ~ NA,
      Diagnosa_06 == "B170 - Acute delta-(super)infection of hepatitis B carrier" ~ NA,
      Diagnosa_06 == "B171 - Acute hepatitis C" ~ NA,
      Diagnosa_06 == "B172 - Acute hepatitis E" ~ NA,
      Diagnosa_06 == "B24 - Unspecified human immunodeficiency virus [HIV] disease" ~ NA,
      Diagnosa_06 == "B260 - Mumps orchitis" ~ NA,
      Diagnosa_06 == "B350 - Tinea barbae and tinea capitis" ~ NA,
      Diagnosa_06 == "B351 - Tinea unguium" ~ NA,
      Diagnosa_06 == "B352 - Tinea manuum" ~ NA,
      Diagnosa_06 == "B353 - Tinea pedis" ~ NA,
      Diagnosa_06 == "B354 - Tinea corporis" ~ NA,
      Diagnosa_06 == "B356 - Tinea cruris" ~ NA,
      Diagnosa_06 == "B359 - Dermatophytosis, unspecified" ~ NA,
      Diagnosa_06 == "B360 - Pityriasis versicolor" ~ NA,
      Diagnosa_06 == "B500 - Plasmodium falciparum malaria with cerebral complications" ~ NA,
      Diagnosa_06 == "B510 - Plasmodium vivax malaria with rupture of spleen" ~ NA,
      Diagnosa_06 == "B520 - Plasmodium malariae malaria with nephropathy" ~ NA,
      Diagnosa_06 == "B530 - Plasmodium ovale malaria" ~ NA,
      Diagnosa_06 == "B54 - Unspecified malaria" ~ NA,
      Diagnosa_06 == "B650 - Schistosomiasis due to Schistosoma haematobium [urinary schistosomiasis]" ~ NA,
      Diagnosa_06 == "B680 - Von Willebrand disease" ~ NA,
      Diagnosa_06 == "B740 - Filariasis due to Wuchereria bancrofti" ~ NA,
      Diagnosa_06 == "B760 - Ancylostomiasis" ~ NA,
      Diagnosa_06 == "B769 - Hookworm disease, unspecified" ~ NA,
      Diagnosa_06 == "B770 - Ascariasis with intestinal complications" ~ NA,
      Diagnosa_06 == "B780 - Intestinal strongyloidiasis" ~ NA,
      Diagnosa_06 == "B850 - Pediculosis due to Pediculus humanus capitis" ~ NA,
      Diagnosa_06 == "B853 - Phthiriasis" ~ NA,
      Diagnosa_06 == "B86 - Scabies" ~ NA,
      Diagnosa_06 == "D179 - Benign lipomatous neoplasm, unspecified" ~ NA,
      Diagnosa_06 == "D500 - Iron deficiency anaemia secondary to blood loss (chronic)" ~ NA,
      Diagnosa_06 == "D649 - Anaemia, unspecified" ~ NA,
      Diagnosa_06 == "E100 - Insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_06 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_06 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ NA,
      Diagnosa_06 == "E162 - Hypoglycaemia, unspecified" ~ NA,
      Diagnosa_06 == "E46 - Unspecified protein-energy malnutrition" ~ NA,
      Diagnosa_06 == "E569 - Vitamin deficiency, unspecified" ~ NA,
      Diagnosa_06 == "E610 - Copper deficiency" ~ NA,
      Diagnosa_06 == "E660 - Obesity due to excess calories" ~ NA,
      Diagnosa_06 == "E740 - Glycogen storage disease" ~ NA,
      Diagnosa_06 == "E785 - Hyperlipidaemia, unspecified" ~ NA,
      Diagnosa_06 == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease" ~ NA,
      Diagnosa_06 == "F450 - Somatization disorder" ~ NA,
      Diagnosa_06 == "G430 - Migraine without aura [common migraine]" ~ NA,
      Diagnosa_06 == "G442 - Tension-type headache" ~ NA,
      Diagnosa_06 == "G470 - Disorders of initiating and maintaining sleep [insomnias]" ~ NA,
      Diagnosa_06 == "G510 - Bell's palsy" ~ NA,
      Diagnosa_06 == "H000 - Hordeolum and other deep inflammation of eyelid" ~ NA,
      Diagnosa_06 == "H010 - Blepharitis" ~ NA,
      Diagnosa_06 == "H020 - Entropion and trichiasis of eyelid" ~ NA,
      Diagnosa_06 == "H100 - Mucopurulent conjunctivitis" ~ NA,
      Diagnosa_06 == "H113 - Conjunctival haemorrhage" ~ NA,
      Diagnosa_06 == "H151 - Episcleritis" ~ NA,
      Diagnosa_06 == "H162 - Keratoconjunctivitis" ~ NA,
      Diagnosa_06 == "H524 - Presbyopia" ~ NA,
      Diagnosa_06 == "H531 - Subjective visual disturbances" ~ NA,
      Diagnosa_06 == "H600 - Abscess of external ear" ~ NA,
      Diagnosa_06 == "H612 - Impacted cerumen" ~ NA,
      Diagnosa_06 == "H669 - Otitis media, unspecified" ~ NA,
      Diagnosa_06 == "H810 - Meniere's disease" ~ NA,
      Diagnosa_06 == "I10 - Essential (primary) hypertension" ~ NA,
      Diagnosa_06 == "I839 - Varicose veins of lower extremities without ulcer or inflammation" ~ NA,
      Diagnosa_06 == "I840 - Internal thrombosed haemorrhoids" ~ NA,
      Diagnosa_06 == "I880 - Nonspecific mesenteric lymphadenitis" ~ NA,
      Diagnosa_06 == "J00 - Acute nasopharyngitis [common cold]" ~ NA,
      Diagnosa_06 == "J020 - Streptococcal pharyngitis" ~ NA,
      Diagnosa_06 == "J030 - Streptococcal tonsillitis" ~ NA,
      Diagnosa_06 == "J040 - Acute laryngitis" ~ NA,
      Diagnosa_06 == "J100 - Influenza with pneumonia, other influenza virus identified" ~ NA,
      Diagnosa_06 == "J120 - Adenoviral pneumonia" ~ NA,
      Diagnosa_06 == "J300 - Vasomotor rhinitis" ~ NA,
      Diagnosa_06 == "J304 - Allergic rhinitis, unspecified" ~ NA,
      Diagnosa_06 == "J340 - Abscess, furuncle and carbuncle of nose" ~ NA,
      Diagnosa_06 == "J40 - Bronchitis, not specified as acute or chronic" ~ NA,
      Diagnosa_06 == "J459 - Asthma, unspecified" ~ NA,
      Diagnosa_06 == "K210 - Gastro-oesophageal reflux disease with oesophagitis" ~ NA,
      Diagnosa_06 == "K291 - Other acute gastritis" ~ NA,
      Diagnosa_06 == "K297 - Gastritis, unspecified" ~ NA,
      Diagnosa_06 == "K30 - Dyspepsia" ~ NA,
      Diagnosa_06 == "K529 - Noninfective gastroenteritis and colitis, unspecified" ~ NA,
      Diagnosa_06 == "L010 - Impeti [any organism] [any site]" ~ NA,
      Diagnosa_06 == "L020 - Cutaneous abscess, furuncle and carbuncle of face" ~ NA,
      Diagnosa_06 == "L022 - Cutaneous abscess, furuncle and carbuncle of trunk" ~ NA,
      Diagnosa_06 == "L028 - Cutaneous abscess, furuncle and carbuncle of other sites" ~ NA,
      Diagnosa_06 == "L080 - Pyoderma" ~ NA,
      Diagnosa_06 == "L081 - Erythrasma" ~ NA,
      Diagnosa_06 == "L089 - Local infection of skin and subcutaneous tissue, unspecified" ~ NA,
      Diagnosa_06 == "L200 - Besnier's pruri" ~ NA,
      Diagnosa_06 == "L210 - Seborrhoea capitis" ~ NA,
      Diagnosa_06 == "L240 - Irritant contact dermatitis due to detergents" ~ NA,
      Diagnosa_06 == "L270 - Generalized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_06 == "L271 - Localized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_06 == "L272 - Dermatitis due to ingested food" ~ NA,
      Diagnosa_06 == "L300 - Nummular dermatitis" ~ NA,
      Diagnosa_06 == "L309 - Dermatitis, unspecified" ~ NA,
      Diagnosa_06 == "L42 - Pityriasis rosea" ~ NA,
      Diagnosa_06 == "L500 - Allergic urticaria" ~ NA,
      Diagnosa_06 == "L700 - Acne vulgaris" ~ NA,
      Diagnosa_06 == "L710 - Perioral dermatitis" ~ NA,
      Diagnosa_06 == "L732 - Hidradenitis suppurativa" ~ NA,
      Diagnosa_06 == "L743 - Miliaria, unspecified" ~ NA,
      Diagnosa_06 == "N12 - Tubulo-interstitial nephritis, not specified as acute or chronic" ~ NA,
      Diagnosa_06 == "N390 - Urinary tract infection, site not specified" ~ NA,
      Diagnosa_06 == "N47 - Redundant prepuce, phimosis and paraphimosis" ~ NA,
      Diagnosa_06 == "N508 - Other specified disorders of male genital organs" ~ NA,
      Diagnosa_06 == "N61 - Inflammatory disorders of breast" ~ NA,
      Diagnosa_06 == "N645 - Other signs and symptoms in breast" ~ NA,
      Diagnosa_06 == "N700 - Acute salpingitis and oophoritis" ~ NA,
      Diagnosa_06 == "N760 - Acute vaginitis" ~ NA,
      Diagnosa_06 == "N762 - Acute vulvitis" ~ NA,
      Diagnosa_06 == "N768 - Other specified inflammation of vagina and vulva" ~ NA,
      Diagnosa_06 == "O000 - Abdominal pregnancy" ~ NA,
      Diagnosa_06 == "O030 - Spontaneous abortion, incomplete, complicated by genital tract and pelvic infection" ~ NA,
      Diagnosa_06 == "O25 - Malnutrition in pregnancy" ~ NA,
      Diagnosa_06 == "O700 - First degree perineal laceration during delivery" ~ NA,
      Diagnosa_06 == "O701 - Second degree perineal laceration during delivery" ~ NA,
      Diagnosa_06 == "O702 - Third degree perineal laceration during delivery" ~ NA,
      Diagnosa_06 == "O703 - Fourth degree perineal laceration during delivery" ~ NA,
      Diagnosa_06 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_06 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_06 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_06 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_06 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_06 == "O921 - Cracked nipple associated with childbirth" ~ NA,
      Diagnosa_06 == "R040 - Epistaxis" ~ NA,
      Diagnosa_06 == "R560 - Febrile convulsions" ~ NA,
      Diagnosa_06 == "R51 - Headache" ~ NA,
      Diagnosa_06 == "T151 - Foreign body in conjunctival sac" ~ NA,
      Diagnosa_06 == "T171 - Foreign body in nostril" ~ NA,
      Diagnosa_06 == "T301 - Burn of first degree, body region unspecified" ~ NA,
      Diagnosa_06 == "T302 - Burn of second degree, body region unspecified" ~ NA,
      Diagnosa_06 == "T753 - Motion sickness" ~ NA,
      Diagnosa_06 == "T782 - Anaphylactic shock, unspecified" ~ NA,
      Diagnosa_06 == "T812 - Accidental puncture and laceration during a procedure, not elsewhere classified" ~ NA,
      Diagnosa_06 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_06 == "Z370 - Single live birth" ~ NA,
      TRUE ~ Diagnosa_06)) %>%
  select(-Diagnosa_06)

nonspes <- nonspes %>%
  mutate(
    Diagnosa_7 = case_when(
      Diagnosa_07 == "A010 - Typhoid fever" ~ NA,
      Diagnosa_07 == "A059 - Bacterial foodborne intoxication, unspecified" ~ NA,
      Diagnosa_07 == "A060 - Acute amoebic dysentery" ~ NA,
      Diagnosa_07 == "A150 - Tuberculosis of lung, confirmed by sputum microscopy with or without culture" ~ NA,
      Diagnosa_07 == "A184 - Tuberculosis of skin and subcutaneous tissue" ~ NA,
      Diagnosa_07 == "A270 - Leptospirosis icterohaemorrhagica" ~ NA,
      Diagnosa_07 == "A300 - Indeterminate leprosy" ~ NA,
      Diagnosa_07 == "A35 - Other tetanus" ~ NA,
      Diagnosa_07 == "A370 - Whooping cough due to Bordetella pertussis" ~ NA,
      Diagnosa_07 == "A46 - Erysipelas" ~ NA,
      Diagnosa_07 == "A510 - Primary genital syphilis" ~ NA,
      Diagnosa_07 == "A520 - Cardiovascular syphilis" ~ NA,
      Diagnosa_07 == "A540 - nococcal infection of lower genitourinary tract without periurethral or accessory gland abscess" ~ NA,
      Diagnosa_07 == "A90 - Dengue fever [classical dengue]" ~ NA,
      Diagnosa_07 == "A91 - Dengue haemorrhagic fever" ~ NA,
      Diagnosa_07 == "A920 - Chikungunya virus disease" ~ NA,
      Diagnosa_07 == "B001 - Herpesviral vesicular dermatitis" ~ NA,
      Diagnosa_07 == "B009 - Herpesviral infection, unspecified" ~ NA,
      Diagnosa_07 == "B019 - Varicella without complication" ~ NA,
      Diagnosa_07 == "B029 - Zoster without complication" ~ NA,
      Diagnosa_07 == "B059 - Measles without complication" ~ NA,
      Diagnosa_07 == "B07 - Viral warts" ~ NA,
      Diagnosa_07 == "B081 - Molluscum contagiosum" ~ NA,
      Diagnosa_07 == "B150 - Hepatitis A with hepatic coma" ~ NA,
      Diagnosa_07 == "B160 - Acute hepatitis B with delta-agent (coinfection) with hepatic coma" ~ NA,
      Diagnosa_07 == "B170 - Acute delta-(super)infection of hepatitis B carrier" ~ NA,
      Diagnosa_07 == "B171 - Acute hepatitis C" ~ NA,
      Diagnosa_07 == "B172 - Acute hepatitis E" ~ NA,
      Diagnosa_07 == "B24 - Unspecified human immunodeficiency virus [HIV] disease" ~ NA,
      Diagnosa_07 == "B260 - Mumps orchitis" ~ NA,
      Diagnosa_07 == "B350 - Tinea barbae and tinea capitis" ~ NA,
      Diagnosa_07 == "B351 - Tinea unguium" ~ NA,
      Diagnosa_07 == "B352 - Tinea manuum" ~ NA,
      Diagnosa_07 == "B353 - Tinea pedis" ~ NA,
      Diagnosa_07 == "B354 - Tinea corporis" ~ NA,
      Diagnosa_07 == "B356 - Tinea cruris" ~ NA,
      Diagnosa_07 == "B359 - Dermatophytosis, unspecified" ~ NA,
      Diagnosa_07 == "B360 - Pityriasis versicolor" ~ NA,
      Diagnosa_07 == "B500 - Plasmodium falciparum malaria with cerebral complications" ~ NA,
      Diagnosa_07 == "B510 - Plasmodium vivax malaria with rupture of spleen" ~ NA,
      Diagnosa_07 == "B520 - Plasmodium malariae malaria with nephropathy" ~ NA,
      Diagnosa_07 == "B530 - Plasmodium ovale malaria" ~ NA,
      Diagnosa_07 == "B54 - Unspecified malaria" ~ NA,
      Diagnosa_07 == "B650 - Schistosomiasis due to Schistosoma haematobium [urinary schistosomiasis]" ~ NA,
      Diagnosa_07 == "B680 - Von Willebrand disease" ~ NA,
      Diagnosa_07 == "B740 - Filariasis due to Wuchereria bancrofti" ~ NA,
      Diagnosa_07 == "B760 - Ancylostomiasis" ~ NA,
      Diagnosa_07 == "B769 - Hookworm disease, unspecified" ~ NA,
      Diagnosa_07 == "B770 - Ascariasis with intestinal complications" ~ NA,
      Diagnosa_07 == "B780 - Intestinal strongyloidiasis" ~ NA,
      Diagnosa_07 == "B850 - Pediculosis due to Pediculus humanus capitis" ~ NA,
      Diagnosa_07 == "B853 - Phthiriasis" ~ NA,
      Diagnosa_07 == "B86 - Scabies" ~ NA,
      Diagnosa_07 == "D179 - Benign lipomatous neoplasm, unspecified" ~ NA,
      Diagnosa_07 == "D500 - Iron deficiency anaemia secondary to blood loss (chronic)" ~ NA,
      Diagnosa_07 == "D649 - Anaemia, unspecified" ~ NA,
      Diagnosa_07 == "E100 - Insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_07 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_07 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ NA,
      Diagnosa_07 == "E162 - Hypoglycaemia, unspecified" ~ NA,
      Diagnosa_07 == "E46 - Unspecified protein-energy malnutrition" ~ NA,
      Diagnosa_07 == "E569 - Vitamin deficiency, unspecified" ~ NA,
      Diagnosa_07 == "E610 - Copper deficiency" ~ NA,
      Diagnosa_07 == "E660 - Obesity due to excess calories" ~ NA,
      Diagnosa_07 == "E740 - Glycogen storage disease" ~ NA,
      Diagnosa_07 == "E785 - Hyperlipidaemia, unspecified" ~ NA,
      Diagnosa_07 == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease" ~ NA,
      Diagnosa_07 == "F450 - Somatization disorder" ~ NA,
      Diagnosa_07 == "G430 - Migraine without aura [common migraine]" ~ NA,
      Diagnosa_07 == "G442 - Tension-type headache" ~ NA,
      Diagnosa_07 == "G470 - Disorders of initiating and maintaining sleep [insomnias]" ~ NA,
      Diagnosa_07 == "G510 - Bell's palsy" ~ NA,
      Diagnosa_07 == "H000 - Hordeolum and other deep inflammation of eyelid" ~ NA,
      Diagnosa_07 == "H010 - Blepharitis" ~ NA,
      Diagnosa_07 == "H020 - Entropion and trichiasis of eyelid" ~ NA,
      Diagnosa_07 == "H100 - Mucopurulent conjunctivitis" ~ NA,
      Diagnosa_07 == "H113 - Conjunctival haemorrhage" ~ NA,
      Diagnosa_07 == "H151 - Episcleritis" ~ NA,
      Diagnosa_07 == "H162 - Keratoconjunctivitis" ~ NA,
      Diagnosa_07 == "H524 - Presbyopia" ~ NA,
      Diagnosa_07 == "H531 - Subjective visual disturbances" ~ NA,
      Diagnosa_07 == "H600 - Abscess of external ear" ~ NA,
      Diagnosa_07 == "H612 - Impacted cerumen" ~ NA,
      Diagnosa_07 == "H669 - Otitis media, unspecified" ~ NA,
      Diagnosa_07 == "H810 - Meniere's disease" ~ NA,
      Diagnosa_07 == "I10 - Essential (primary) hypertension" ~ NA,
      Diagnosa_07 == "I839 - Varicose veins of lower extremities without ulcer or inflammation" ~ NA,
      Diagnosa_07 == "I840 - Internal thrombosed haemorrhoids" ~ NA,
      Diagnosa_07 == "I880 - Nonspecific mesenteric lymphadenitis" ~ NA,
      Diagnosa_07 == "J00 - Acute nasopharyngitis [common cold]" ~ NA,
      Diagnosa_07 == "J020 - Streptococcal pharyngitis" ~ NA,
      Diagnosa_07 == "J030 - Streptococcal tonsillitis" ~ NA,
      Diagnosa_07 == "J040 - Acute laryngitis" ~ NA,
      Diagnosa_07 == "J100 - Influenza with pneumonia, other influenza virus identified" ~ NA,
      Diagnosa_07 == "J120 - Adenoviral pneumonia" ~ NA,
      Diagnosa_07 == "J300 - Vasomotor rhinitis" ~ NA,
      Diagnosa_07 == "J304 - Allergic rhinitis, unspecified" ~ NA,
      Diagnosa_07 == "J340 - Abscess, furuncle and carbuncle of nose" ~ NA,
      Diagnosa_07 == "J40 - Bronchitis, not specified as acute or chronic" ~ NA,
      Diagnosa_07 == "J459 - Asthma, unspecified" ~ NA,
      Diagnosa_07 == "K210 - Gastro-oesophageal reflux disease with oesophagitis" ~ NA,
      Diagnosa_07 == "K291 - Other acute gastritis" ~ NA,
      Diagnosa_07 == "K297 - Gastritis, unspecified" ~ NA,
      Diagnosa_07 == "K30 - Dyspepsia" ~ NA,
      Diagnosa_07 == "K529 - Noninfective gastroenteritis and colitis, unspecified" ~ NA,
      Diagnosa_07 == "L010 - Impeti [any organism] [any site]" ~ NA,
      Diagnosa_07 == "L020 - Cutaneous abscess, furuncle and carbuncle of face" ~ NA,
      Diagnosa_07 == "L022 - Cutaneous abscess, furuncle and carbuncle of trunk" ~ NA,
      Diagnosa_07 == "L028 - Cutaneous abscess, furuncle and carbuncle of other sites" ~ NA,
      Diagnosa_07 == "L080 - Pyoderma" ~ NA,
      Diagnosa_07 == "L081 - Erythrasma" ~ NA,
      Diagnosa_07 == "L089 - Local infection of skin and subcutaneous tissue, unspecified" ~ NA,
      Diagnosa_07 == "L200 - Besnier's pruri" ~ NA,
      Diagnosa_07 == "L210 - Seborrhoea capitis" ~ NA,
      Diagnosa_07 == "L240 - Irritant contact dermatitis due to detergents" ~ NA,
      Diagnosa_07 == "L270 - Generalized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_07 == "L271 - Localized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_07 == "L272 - Dermatitis due to ingested food" ~ NA,
      Diagnosa_07 == "L300 - Nummular dermatitis" ~ NA,
      Diagnosa_07 == "L309 - Dermatitis, unspecified" ~ NA,
      Diagnosa_07 == "L42 - Pityriasis rosea" ~ NA,
      Diagnosa_07 == "L500 - Allergic urticaria" ~ NA,
      Diagnosa_07 == "L700 - Acne vulgaris" ~ NA,
      Diagnosa_07 == "L710 - Perioral dermatitis" ~ NA,
      Diagnosa_07 == "L732 - Hidradenitis suppurativa" ~ NA,
      Diagnosa_07 == "L743 - Miliaria, unspecified" ~ NA,
      Diagnosa_07 == "N12 - Tubulo-interstitial nephritis, not specified as acute or chronic" ~ NA,
      Diagnosa_07 == "N390 - Urinary tract infection, site not specified" ~ NA,
      Diagnosa_07 == "N47 - Redundant prepuce, phimosis and paraphimosis" ~ NA,
      Diagnosa_07 == "N508 - Other specified disorders of male genital organs" ~ NA,
      Diagnosa_07 == "N61 - Inflammatory disorders of breast" ~ NA,
      Diagnosa_07 == "N645 - Other signs and symptoms in breast" ~ NA,
      Diagnosa_07 == "N700 - Acute salpingitis and oophoritis" ~ NA,
      Diagnosa_07 == "N760 - Acute vaginitis" ~ NA,
      Diagnosa_07 == "N762 - Acute vulvitis" ~ NA,
      Diagnosa_07 == "N768 - Other specified inflammation of vagina and vulva" ~ NA,
      Diagnosa_07 == "O000 - Abdominal pregnancy" ~ NA,
      Diagnosa_07 == "O030 - Spontaneous abortion, incomplete, complicated by genital tract and pelvic infection" ~ NA,
      Diagnosa_07 == "O25 - Malnutrition in pregnancy" ~ NA,
      Diagnosa_07 == "O700 - First degree perineal laceration during delivery" ~ NA,
      Diagnosa_07 == "O701 - Second degree perineal laceration during delivery" ~ NA,
      Diagnosa_07 == "O702 - Third degree perineal laceration during delivery" ~ NA,
      Diagnosa_07 == "O703 - Fourth degree perineal laceration during delivery" ~ NA,
      Diagnosa_07 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_07 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_07 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_07 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_07 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_07 == "O921 - Cracked nipple associated with childbirth" ~ NA,
      Diagnosa_07 == "R040 - Epistaxis" ~ NA,
      Diagnosa_07 == "R560 - Febrile convulsions" ~ NA,
      Diagnosa_07 == "R51 - Headache" ~ NA,
      Diagnosa_07 == "T151 - Foreign body in conjunctival sac" ~ NA,
      Diagnosa_07 == "T171 - Foreign body in nostril" ~ NA,
      Diagnosa_07 == "T301 - Burn of first degree, body region unspecified" ~ NA,
      Diagnosa_07 == "T302 - Burn of second degree, body region unspecified" ~ NA,
      Diagnosa_07 == "T753 - Motion sickness" ~ NA,
      Diagnosa_07 == "T782 - Anaphylactic shock, unspecified" ~ NA,
      Diagnosa_07 == "T812 - Accidental puncture and laceration during a procedure, not elsewhere classified" ~ NA,
      Diagnosa_07 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_07 == "Z370 - Single live birth" ~ NA,
      TRUE ~ Diagnosa_07)) %>%
  select(-Diagnosa_07)

nonspes <- nonspes %>%
  mutate(
    Diagnosa_8 = case_when(
      Diagnosa_08 == "A010 - Typhoid fever" ~ NA,
      Diagnosa_08 == "A059 - Bacterial foodborne intoxication, unspecified" ~ NA,
      Diagnosa_08 == "A060 - Acute amoebic dysentery" ~ NA,
      Diagnosa_08 == "A150 - Tuberculosis of lung, confirmed by sputum microscopy with or without culture" ~ NA,
      Diagnosa_08 == "A184 - Tuberculosis of skin and subcutaneous tissue" ~ NA,
      Diagnosa_08 == "A270 - Leptospirosis icterohaemorrhagica" ~ NA,
      Diagnosa_08 == "A300 - Indeterminate leprosy" ~ NA,
      Diagnosa_08 == "A35 - Other tetanus" ~ NA,
      Diagnosa_08 == "A370 - Whooping cough due to Bordetella pertussis" ~ NA,
      Diagnosa_08 == "A46 - Erysipelas" ~ NA,
      Diagnosa_08 == "A510 - Primary genital syphilis" ~ NA,
      Diagnosa_08 == "A520 - Cardiovascular syphilis" ~ NA,
      Diagnosa_08 == "A540 - nococcal infection of lower genitourinary tract without periurethral or accessory gland abscess" ~ NA,
      Diagnosa_08 == "A90 - Dengue fever [classical dengue]" ~ NA,
      Diagnosa_08 == "A91 - Dengue haemorrhagic fever" ~ NA,
      Diagnosa_08 == "A920 - Chikungunya virus disease" ~ NA,
      Diagnosa_08 == "B001 - Herpesviral vesicular dermatitis" ~ NA,
      Diagnosa_08 == "B009 - Herpesviral infection, unspecified" ~ NA,
      Diagnosa_08 == "B019 - Varicella without complication" ~ NA,
      Diagnosa_08 == "B029 - Zoster without complication" ~ NA,
      Diagnosa_08 == "B059 - Measles without complication" ~ NA,
      Diagnosa_08 == "B07 - Viral warts" ~ NA,
      Diagnosa_08 == "B081 - Molluscum contagiosum" ~ NA,
      Diagnosa_08 == "B150 - Hepatitis A with hepatic coma" ~ NA,
      Diagnosa_08 == "B160 - Acute hepatitis B with delta-agent (coinfection) with hepatic coma" ~ NA,
      Diagnosa_08 == "B170 - Acute delta-(super)infection of hepatitis B carrier" ~ NA,
      Diagnosa_08 == "B171 - Acute hepatitis C" ~ NA,
      Diagnosa_08 == "B172 - Acute hepatitis E" ~ NA,
      Diagnosa_08 == "B24 - Unspecified human immunodeficiency virus [HIV] disease" ~ NA,
      Diagnosa_08 == "B260 - Mumps orchitis" ~ NA,
      Diagnosa_08 == "B350 - Tinea barbae and tinea capitis" ~ NA,
      Diagnosa_08 == "B351 - Tinea unguium" ~ NA,
      Diagnosa_08 == "B352 - Tinea manuum" ~ NA,
      Diagnosa_08 == "B353 - Tinea pedis" ~ NA,
      Diagnosa_08 == "B354 - Tinea corporis" ~ NA,
      Diagnosa_08 == "B356 - Tinea cruris" ~ NA,
      Diagnosa_08 == "B359 - Dermatophytosis, unspecified" ~ NA,
      Diagnosa_08 == "B360 - Pityriasis versicolor" ~ NA,
      Diagnosa_08 == "B500 - Plasmodium falciparum malaria with cerebral complications" ~ NA,
      Diagnosa_08 == "B510 - Plasmodium vivax malaria with rupture of spleen" ~ NA,
      Diagnosa_08 == "B520 - Plasmodium malariae malaria with nephropathy" ~ NA,
      Diagnosa_08 == "B530 - Plasmodium ovale malaria" ~ NA,
      Diagnosa_08 == "B54 - Unspecified malaria" ~ NA,
      Diagnosa_08 == "B650 - Schistosomiasis due to Schistosoma haematobium [urinary schistosomiasis]" ~ NA,
      Diagnosa_08 == "B680 - Von Willebrand disease" ~ NA,
      Diagnosa_08 == "B740 - Filariasis due to Wuchereria bancrofti" ~ NA,
      Diagnosa_08 == "B760 - Ancylostomiasis" ~ NA,
      Diagnosa_08 == "B769 - Hookworm disease, unspecified" ~ NA,
      Diagnosa_08 == "B770 - Ascariasis with intestinal complications" ~ NA,
      Diagnosa_08 == "B780 - Intestinal strongyloidiasis" ~ NA,
      Diagnosa_08 == "B850 - Pediculosis due to Pediculus humanus capitis" ~ NA,
      Diagnosa_08 == "B853 - Phthiriasis" ~ NA,
      Diagnosa_08 == "B86 - Scabies" ~ NA,
      Diagnosa_08 == "D179 - Benign lipomatous neoplasm, unspecified" ~ NA,
      Diagnosa_08 == "D500 - Iron deficiency anaemia secondary to blood loss (chronic)" ~ NA,
      Diagnosa_08 == "D649 - Anaemia, unspecified" ~ NA,
      Diagnosa_08 == "E100 - Insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_08 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_08 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ NA,
      Diagnosa_08 == "E162 - Hypoglycaemia, unspecified" ~ NA,
      Diagnosa_08 == "E46 - Unspecified protein-energy malnutrition" ~ NA,
      Diagnosa_08 == "E569 - Vitamin deficiency, unspecified" ~ NA,
      Diagnosa_08 == "E610 - Copper deficiency" ~ NA,
      Diagnosa_08 == "E660 - Obesity due to excess calories" ~ NA,
      Diagnosa_08 == "E740 - Glycogen storage disease" ~ NA,
      Diagnosa_08 == "E785 - Hyperlipidaemia, unspecified" ~ NA,
      Diagnosa_08 == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease" ~ NA,
      Diagnosa_08 == "F450 - Somatization disorder" ~ NA,
      Diagnosa_08 == "G430 - Migraine without aura [common migraine]" ~ NA,
      Diagnosa_08 == "G442 - Tension-type headache" ~ NA,
      Diagnosa_08 == "G470 - Disorders of initiating and maintaining sleep [insomnias]" ~ NA,
      Diagnosa_08 == "G510 - Bell's palsy" ~ NA,
      Diagnosa_08 == "H000 - Hordeolum and other deep inflammation of eyelid" ~ NA,
      Diagnosa_08 == "H010 - Blepharitis" ~ NA,
      Diagnosa_08 == "H020 - Entropion and trichiasis of eyelid" ~ NA,
      Diagnosa_08 == "H100 - Mucopurulent conjunctivitis" ~ NA,
      Diagnosa_08 == "H113 - Conjunctival haemorrhage" ~ NA,
      Diagnosa_08 == "H151 - Episcleritis" ~ NA,
      Diagnosa_08 == "H162 - Keratoconjunctivitis" ~ NA,
      Diagnosa_08 == "H524 - Presbyopia" ~ NA,
      Diagnosa_08 == "H531 - Subjective visual disturbances" ~ NA,
      Diagnosa_08 == "H600 - Abscess of external ear" ~ NA,
      Diagnosa_08 == "H612 - Impacted cerumen" ~ NA,
      Diagnosa_08 == "H669 - Otitis media, unspecified" ~ NA,
      Diagnosa_08 == "H810 - Meniere's disease" ~ NA,
      Diagnosa_08 == "I10 - Essential (primary) hypertension" ~ NA,
      Diagnosa_08 == "I839 - Varicose veins of lower extremities without ulcer or inflammation" ~ NA,
      Diagnosa_08 == "I840 - Internal thrombosed haemorrhoids" ~ NA,
      Diagnosa_08 == "I880 - Nonspecific mesenteric lymphadenitis" ~ NA,
      Diagnosa_08 == "J00 - Acute nasopharyngitis [common cold]" ~ NA,
      Diagnosa_08 == "J020 - Streptococcal pharyngitis" ~ NA,
      Diagnosa_08 == "J030 - Streptococcal tonsillitis" ~ NA,
      Diagnosa_08 == "J040 - Acute laryngitis" ~ NA,
      Diagnosa_08 == "J100 - Influenza with pneumonia, other influenza virus identified" ~ NA,
      Diagnosa_08 == "J120 - Adenoviral pneumonia" ~ NA,
      Diagnosa_08 == "J300 - Vasomotor rhinitis" ~ NA,
      Diagnosa_08 == "J304 - Allergic rhinitis, unspecified" ~ NA,
      Diagnosa_08 == "J340 - Abscess, furuncle and carbuncle of nose" ~ NA,
      Diagnosa_08 == "J40 - Bronchitis, not specified as acute or chronic" ~ NA,
      Diagnosa_08 == "J459 - Asthma, unspecified" ~ NA,
      Diagnosa_08 == "K210 - Gastro-oesophageal reflux disease with oesophagitis" ~ NA,
      Diagnosa_08 == "K291 - Other acute gastritis" ~ NA,
      Diagnosa_08 == "K297 - Gastritis, unspecified" ~ NA,
      Diagnosa_08 == "K30 - Dyspepsia" ~ NA,
      Diagnosa_08 == "K529 - Noninfective gastroenteritis and colitis, unspecified" ~ NA,
      Diagnosa_08 == "L010 - Impeti [any organism] [any site]" ~ NA,
      Diagnosa_08 == "L020 - Cutaneous abscess, furuncle and carbuncle of face" ~ NA,
      Diagnosa_08 == "L022 - Cutaneous abscess, furuncle and carbuncle of trunk" ~ NA,
      Diagnosa_08 == "L028 - Cutaneous abscess, furuncle and carbuncle of other sites" ~ NA,
      Diagnosa_08 == "L080 - Pyoderma" ~ NA,
      Diagnosa_08 == "L081 - Erythrasma" ~ NA,
      Diagnosa_08 == "L089 - Local infection of skin and subcutaneous tissue, unspecified" ~ NA,
      Diagnosa_08 == "L200 - Besnier's pruri" ~ NA,
      Diagnosa_08 == "L210 - Seborrhoea capitis" ~ NA,
      Diagnosa_08 == "L240 - Irritant contact dermatitis due to detergents" ~ NA,
      Diagnosa_08 == "L270 - Generalized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_08 == "L271 - Localized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_08 == "L272 - Dermatitis due to ingested food" ~ NA,
      Diagnosa_08 == "L300 - Nummular dermatitis" ~ NA,
      Diagnosa_08 == "L309 - Dermatitis, unspecified" ~ NA,
      Diagnosa_08 == "L42 - Pityriasis rosea" ~ NA,
      Diagnosa_08 == "L500 - Allergic urticaria" ~ NA,
      Diagnosa_08 == "L700 - Acne vulgaris" ~ NA,
      Diagnosa_08 == "L710 - Perioral dermatitis" ~ NA,
      Diagnosa_08 == "L732 - Hidradenitis suppurativa" ~ NA,
      Diagnosa_08 == "L743 - Miliaria, unspecified" ~ NA,
      Diagnosa_08 == "N12 - Tubulo-interstitial nephritis, not specified as acute or chronic" ~ NA,
      Diagnosa_08 == "N390 - Urinary tract infection, site not specified" ~ NA,
      Diagnosa_08 == "N47 - Redundant prepuce, phimosis and paraphimosis" ~ NA,
      Diagnosa_08 == "N508 - Other specified disorders of male genital organs" ~ NA,
      Diagnosa_08 == "N61 - Inflammatory disorders of breast" ~ NA,
      Diagnosa_08 == "N645 - Other signs and symptoms in breast" ~ NA,
      Diagnosa_08 == "N700 - Acute salpingitis and oophoritis" ~ NA,
      Diagnosa_08 == "N760 - Acute vaginitis" ~ NA,
      Diagnosa_08 == "N762 - Acute vulvitis" ~ NA,
      Diagnosa_08 == "N768 - Other specified inflammation of vagina and vulva" ~ NA,
      Diagnosa_08 == "O000 - Abdominal pregnancy" ~ NA,
      Diagnosa_08 == "O030 - Spontaneous abortion, incomplete, complicated by genital tract and pelvic infection" ~ NA,
      Diagnosa_08 == "O25 - Malnutrition in pregnancy" ~ NA,
      Diagnosa_08 == "O700 - First degree perineal laceration during delivery" ~ NA,
      Diagnosa_08 == "O701 - Second degree perineal laceration during delivery" ~ NA,
      Diagnosa_08 == "O702 - Third degree perineal laceration during delivery" ~ NA,
      Diagnosa_08 == "O703 - Fourth degree perineal laceration during delivery" ~ NA,
      Diagnosa_08 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_08 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_08 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_08 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_08 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_08 == "O921 - Cracked nipple associated with childbirth" ~ NA,
      Diagnosa_08 == "R040 - Epistaxis" ~ NA,
      Diagnosa_08 == "R560 - Febrile convulsions" ~ NA,
      Diagnosa_08 == "R51 - Headache" ~ NA,
      Diagnosa_08 == "T151 - Foreign body in conjunctival sac" ~ NA,
      Diagnosa_08 == "T171 - Foreign body in nostril" ~ NA,
      Diagnosa_08 == "T301 - Burn of first degree, body region unspecified" ~ NA,
      Diagnosa_08 == "T302 - Burn of second degree, body region unspecified" ~ NA,
      Diagnosa_08 == "T753 - Motion sickness" ~ NA,
      Diagnosa_08 == "T782 - Anaphylactic shock, unspecified" ~ NA,
      Diagnosa_08 == "T812 - Accidental puncture and laceration during a procedure, not elsewhere classified" ~ NA,
      Diagnosa_08 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_08 == "Z370 - Single live birth" ~ NA,
      TRUE ~ Diagnosa_08)) %>%
  select(-Diagnosa_08)

nonspes <- nonspes %>%
  mutate(
    Diagnosa_9 = case_when(
      Diagnosa_09 == "A010 - Typhoid fever" ~ NA,
      Diagnosa_09 == "A059 - Bacterial foodborne intoxication, unspecified" ~ NA,
      Diagnosa_09 == "A060 - Acute amoebic dysentery" ~ NA,
      Diagnosa_09 == "A150 - Tuberculosis of lung, confirmed by sputum microscopy with or without culture" ~ NA,
      Diagnosa_09 == "A184 - Tuberculosis of skin and subcutaneous tissue" ~ NA,
      Diagnosa_09 == "A270 - Leptospirosis icterohaemorrhagica" ~ NA,
      Diagnosa_09 == "A300 - Indeterminate leprosy" ~ NA,
      Diagnosa_09 == "A35 - Other tetanus" ~ NA,
      Diagnosa_09 == "A370 - Whooping cough due to Bordetella pertussis" ~ NA,
      Diagnosa_09 == "A46 - Erysipelas" ~ NA,
      Diagnosa_09 == "A510 - Primary genital syphilis" ~ NA,
      Diagnosa_09 == "A520 - Cardiovascular syphilis" ~ NA,
      Diagnosa_09 == "A540 - nococcal infection of lower genitourinary tract without periurethral or accessory gland abscess" ~ NA,
      Diagnosa_09 == "A90 - Dengue fever [classical dengue]" ~ NA,
      Diagnosa_09 == "A91 - Dengue haemorrhagic fever" ~ NA,
      Diagnosa_09 == "A920 - Chikungunya virus disease" ~ NA,
      Diagnosa_09 == "B001 - Herpesviral vesicular dermatitis" ~ NA,
      Diagnosa_09 == "B009 - Herpesviral infection, unspecified" ~ NA,
      Diagnosa_09 == "B019 - Varicella without complication" ~ NA,
      Diagnosa_09 == "B029 - Zoster without complication" ~ NA,
      Diagnosa_09 == "B059 - Measles without complication" ~ NA,
      Diagnosa_09 == "B07 - Viral warts" ~ NA,
      Diagnosa_09 == "B081 - Molluscum contagiosum" ~ NA,
      Diagnosa_09 == "B150 - Hepatitis A with hepatic coma" ~ NA,
      Diagnosa_09 == "B160 - Acute hepatitis B with delta-agent (coinfection) with hepatic coma" ~ NA,
      Diagnosa_09 == "B170 - Acute delta-(super)infection of hepatitis B carrier" ~ NA,
      Diagnosa_09 == "B171 - Acute hepatitis C" ~ NA,
      Diagnosa_09 == "B172 - Acute hepatitis E" ~ NA,
      Diagnosa_09 == "B24 - Unspecified human immunodeficiency virus [HIV] disease" ~ NA,
      Diagnosa_09 == "B260 - Mumps orchitis" ~ NA,
      Diagnosa_09 == "B350 - Tinea barbae and tinea capitis" ~ NA,
      Diagnosa_09 == "B351 - Tinea unguium" ~ NA,
      Diagnosa_09 == "B352 - Tinea manuum" ~ NA,
      Diagnosa_09 == "B353 - Tinea pedis" ~ NA,
      Diagnosa_09 == "B354 - Tinea corporis" ~ NA,
      Diagnosa_09 == "B356 - Tinea cruris" ~ NA,
      Diagnosa_09 == "B359 - Dermatophytosis, unspecified" ~ NA,
      Diagnosa_09 == "B360 - Pityriasis versicolor" ~ NA,
      Diagnosa_09 == "B500 - Plasmodium falciparum malaria with cerebral complications" ~ NA,
      Diagnosa_09 == "B510 - Plasmodium vivax malaria with rupture of spleen" ~ NA,
      Diagnosa_09 == "B520 - Plasmodium malariae malaria with nephropathy" ~ NA,
      Diagnosa_09 == "B530 - Plasmodium ovale malaria" ~ NA,
      Diagnosa_09 == "B54 - Unspecified malaria" ~ NA,
      Diagnosa_09 == "B650 - Schistosomiasis due to Schistosoma haematobium [urinary schistosomiasis]" ~ NA,
      Diagnosa_09 == "B680 - Von Willebrand disease" ~ NA,
      Diagnosa_09 == "B740 - Filariasis due to Wuchereria bancrofti" ~ NA,
      Diagnosa_09 == "B760 - Ancylostomiasis" ~ NA,
      Diagnosa_09 == "B769 - Hookworm disease, unspecified" ~ NA,
      Diagnosa_09 == "B770 - Ascariasis with intestinal complications" ~ NA,
      Diagnosa_09 == "B780 - Intestinal strongyloidiasis" ~ NA,
      Diagnosa_09 == "B850 - Pediculosis due to Pediculus humanus capitis" ~ NA,
      Diagnosa_09 == "B853 - Phthiriasis" ~ NA,
      Diagnosa_09 == "B86 - Scabies" ~ NA,
      Diagnosa_09 == "D179 - Benign lipomatous neoplasm, unspecified" ~ NA,
      Diagnosa_09 == "D500 - Iron deficiency anaemia secondary to blood loss (chronic)" ~ NA,
      Diagnosa_09 == "D649 - Anaemia, unspecified" ~ NA,
      Diagnosa_09 == "E100 - Insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_09 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_09 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ NA,
      Diagnosa_09 == "E162 - Hypoglycaemia, unspecified" ~ NA,
      Diagnosa_09 == "E46 - Unspecified protein-energy malnutrition" ~ NA,
      Diagnosa_09 == "E569 - Vitamin deficiency, unspecified" ~ NA,
      Diagnosa_09 == "E610 - Copper deficiency" ~ NA,
      Diagnosa_09 == "E660 - Obesity due to excess calories" ~ NA,
      Diagnosa_09 == "E740 - Glycogen storage disease" ~ NA,
      Diagnosa_09 == "E785 - Hyperlipidaemia, unspecified" ~ NA,
      Diagnosa_09 == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease" ~ NA,
      Diagnosa_09 == "F450 - Somatization disorder" ~ NA,
      Diagnosa_09 == "G430 - Migraine without aura [common migraine]" ~ NA,
      Diagnosa_09 == "G442 - Tension-type headache" ~ NA,
      Diagnosa_09 == "G470 - Disorders of initiating and maintaining sleep [insomnias]" ~ NA,
      Diagnosa_09 == "G510 - Bell's palsy" ~ NA,
      Diagnosa_09 == "H000 - Hordeolum and other deep inflammation of eyelid" ~ NA,
      Diagnosa_09 == "H010 - Blepharitis" ~ NA,
      Diagnosa_09 == "H020 - Entropion and trichiasis of eyelid" ~ NA,
      Diagnosa_09 == "H100 - Mucopurulent conjunctivitis" ~ NA,
      Diagnosa_09 == "H113 - Conjunctival haemorrhage" ~ NA,
      Diagnosa_09 == "H151 - Episcleritis" ~ NA,
      Diagnosa_09 == "H162 - Keratoconjunctivitis" ~ NA,
      Diagnosa_09 == "H524 - Presbyopia" ~ NA,
      Diagnosa_09 == "H531 - Subjective visual disturbances" ~ NA,
      Diagnosa_09 == "H600 - Abscess of external ear" ~ NA,
      Diagnosa_09 == "H612 - Impacted cerumen" ~ NA,
      Diagnosa_09 == "H669 - Otitis media, unspecified" ~ NA,
      Diagnosa_09 == "H810 - Meniere's disease" ~ NA,
      Diagnosa_09 == "I10 - Essential (primary) hypertension" ~ NA,
      Diagnosa_09 == "I839 - Varicose veins of lower extremities without ulcer or inflammation" ~ NA,
      Diagnosa_09 == "I840 - Internal thrombosed haemorrhoids" ~ NA,
      Diagnosa_09 == "I880 - Nonspecific mesenteric lymphadenitis" ~ NA,
      Diagnosa_09 == "J00 - Acute nasopharyngitis [common cold]" ~ NA,
      Diagnosa_09 == "J020 - Streptococcal pharyngitis" ~ NA,
      Diagnosa_09 == "J030 - Streptococcal tonsillitis" ~ NA,
      Diagnosa_09 == "J040 - Acute laryngitis" ~ NA,
      Diagnosa_09 == "J100 - Influenza with pneumonia, other influenza virus identified" ~ NA,
      Diagnosa_09 == "J120 - Adenoviral pneumonia" ~ NA,
      Diagnosa_09 == "J300 - Vasomotor rhinitis" ~ NA,
      Diagnosa_09 == "J304 - Allergic rhinitis, unspecified" ~ NA,
      Diagnosa_09 == "J340 - Abscess, furuncle and carbuncle of nose" ~ NA,
      Diagnosa_09 == "J40 - Bronchitis, not specified as acute or chronic" ~ NA,
      Diagnosa_09 == "J459 - Asthma, unspecified" ~ NA,
      Diagnosa_09 == "K210 - Gastro-oesophageal reflux disease with oesophagitis" ~ NA,
      Diagnosa_09 == "K291 - Other acute gastritis" ~ NA,
      Diagnosa_09 == "K297 - Gastritis, unspecified" ~ NA,
      Diagnosa_09 == "K30 - Dyspepsia" ~ NA,
      Diagnosa_09 == "K529 - Noninfective gastroenteritis and colitis, unspecified" ~ NA,
      Diagnosa_09 == "L010 - Impeti [any organism] [any site]" ~ NA,
      Diagnosa_09 == "L020 - Cutaneous abscess, furuncle and carbuncle of face" ~ NA,
      Diagnosa_09 == "L022 - Cutaneous abscess, furuncle and carbuncle of trunk" ~ NA,
      Diagnosa_09 == "L028 - Cutaneous abscess, furuncle and carbuncle of other sites" ~ NA,
      Diagnosa_09 == "L080 - Pyoderma" ~ NA,
      Diagnosa_09 == "L081 - Erythrasma" ~ NA,
      Diagnosa_09 == "L089 - Local infection of skin and subcutaneous tissue, unspecified" ~ NA,
      Diagnosa_09 == "L200 - Besnier's pruri" ~ NA,
      Diagnosa_09 == "L210 - Seborrhoea capitis" ~ NA,
      Diagnosa_09 == "L240 - Irritant contact dermatitis due to detergents" ~ NA,
      Diagnosa_09 == "L270 - Generalized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_09 == "L271 - Localized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_09 == "L272 - Dermatitis due to ingested food" ~ NA,
      Diagnosa_09 == "L300 - Nummular dermatitis" ~ NA,
      Diagnosa_09 == "L309 - Dermatitis, unspecified" ~ NA,
      Diagnosa_09 == "L42 - Pityriasis rosea" ~ NA,
      Diagnosa_09 == "L500 - Allergic urticaria" ~ NA,
      Diagnosa_09 == "L700 - Acne vulgaris" ~ NA,
      Diagnosa_09 == "L710 - Perioral dermatitis" ~ NA,
      Diagnosa_09 == "L732 - Hidradenitis suppurativa" ~ NA,
      Diagnosa_09 == "L743 - Miliaria, unspecified" ~ NA,
      Diagnosa_09 == "N12 - Tubulo-interstitial nephritis, not specified as acute or chronic" ~ NA,
      Diagnosa_09 == "N390 - Urinary tract infection, site not specified" ~ NA,
      Diagnosa_09 == "N47 - Redundant prepuce, phimosis and paraphimosis" ~ NA,
      Diagnosa_09 == "N508 - Other specified disorders of male genital organs" ~ NA,
      Diagnosa_09 == "N61 - Inflammatory disorders of breast" ~ NA,
      Diagnosa_09 == "N645 - Other signs and symptoms in breast" ~ NA,
      Diagnosa_09 == "N700 - Acute salpingitis and oophoritis" ~ NA,
      Diagnosa_09 == "N760 - Acute vaginitis" ~ NA,
      Diagnosa_09 == "N762 - Acute vulvitis" ~ NA,
      Diagnosa_09 == "N768 - Other specified inflammation of vagina and vulva" ~ NA,
      Diagnosa_09 == "O000 - Abdominal pregnancy" ~ NA,
      Diagnosa_09 == "O030 - Spontaneous abortion, incomplete, complicated by genital tract and pelvic infection" ~ NA,
      Diagnosa_09 == "O25 - Malnutrition in pregnancy" ~ NA,
      Diagnosa_09 == "O700 - First degree perineal laceration during delivery" ~ NA,
      Diagnosa_09 == "O701 - Second degree perineal laceration during delivery" ~ NA,
      Diagnosa_09 == "O702 - Third degree perineal laceration during delivery" ~ NA,
      Diagnosa_09 == "O703 - Fourth degree perineal laceration during delivery" ~ NA,
      Diagnosa_09 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_09 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_09 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_09 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_09 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_09 == "O921 - Cracked nipple associated with childbirth" ~ NA,
      Diagnosa_09 == "R040 - Epistaxis" ~ NA,
      Diagnosa_09 == "R560 - Febrile convulsions" ~ NA,
      Diagnosa_09 == "R51 - Headache" ~ NA,
      Diagnosa_09 == "T151 - Foreign body in conjunctival sac" ~ NA,
      Diagnosa_09 == "T171 - Foreign body in nostril" ~ NA,
      Diagnosa_09 == "T301 - Burn of first degree, body region unspecified" ~ NA,
      Diagnosa_09 == "T302 - Burn of second degree, body region unspecified" ~ NA,
      Diagnosa_09 == "T753 - Motion sickness" ~ NA,
      Diagnosa_09 == "T782 - Anaphylactic shock, unspecified" ~ NA,
      Diagnosa_09 == "T812 - Accidental puncture and laceration during a procedure, not elsewhere classified" ~ NA,
      Diagnosa_09 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_09 == "Z370 - Single live birth" ~ NA,
      TRUE ~ Diagnosa_09)) %>%
  select(-Diagnosa_09)

nonspes <- nonspes %>%
  mutate(
    Diagnosa_010 = case_when(
      Diagnosa_10 == "A010 - Typhoid fever" ~ NA,
      Diagnosa_10 == "A059 - Bacterial foodborne intoxication, unspecified" ~ NA,
      Diagnosa_10 == "A060 - Acute amoebic dysentery" ~ NA,
      Diagnosa_10 == "A150 - Tuberculosis of lung, confirmed by sputum microscopy with or without culture" ~ NA,
      Diagnosa_10 == "A184 - Tuberculosis of skin and subcutaneous tissue" ~ NA,
      Diagnosa_10 == "A270 - Leptospirosis icterohaemorrhagica" ~ NA,
      Diagnosa_10 == "A300 - Indeterminate leprosy" ~ NA,
      Diagnosa_10 == "A35 - Other tetanus" ~ NA,
      Diagnosa_10 == "A370 - Whooping cough due to Bordetella pertussis" ~ NA,
      Diagnosa_10 == "A46 - Erysipelas" ~ NA,
      Diagnosa_10 == "A510 - Primary genital syphilis" ~ NA,
      Diagnosa_10 == "A520 - Cardiovascular syphilis" ~ NA,
      Diagnosa_10 == "A540 - nococcal infection of lower genitourinary tract without periurethral or accessory gland abscess" ~ NA,
      Diagnosa_10 == "A90 - Dengue fever [classical dengue]" ~ NA,
      Diagnosa_10 == "A91 - Dengue haemorrhagic fever" ~ NA,
      Diagnosa_10 == "A920 - Chikungunya virus disease" ~ NA,
      Diagnosa_10 == "B001 - Herpesviral vesicular dermatitis" ~ NA,
      Diagnosa_10 == "B009 - Herpesviral infection, unspecified" ~ NA,
      Diagnosa_10 == "B019 - Varicella without complication" ~ NA,
      Diagnosa_10 == "B029 - Zoster without complication" ~ NA,
      Diagnosa_10 == "B059 - Measles without complication" ~ NA,
      Diagnosa_10 == "B07 - Viral warts" ~ NA,
      Diagnosa_10 == "B081 - Molluscum contagiosum" ~ NA,
      Diagnosa_10 == "B150 - Hepatitis A with hepatic coma" ~ NA,
      Diagnosa_10 == "B160 - Acute hepatitis B with delta-agent (coinfection) with hepatic coma" ~ NA,
      Diagnosa_10 == "B170 - Acute delta-(super)infection of hepatitis B carrier" ~ NA,
      Diagnosa_10 == "B171 - Acute hepatitis C" ~ NA,
      Diagnosa_10 == "B172 - Acute hepatitis E" ~ NA,
      Diagnosa_10 == "B24 - Unspecified human immunodeficiency virus [HIV] disease" ~ NA,
      Diagnosa_10 == "B260 - Mumps orchitis" ~ NA,
      Diagnosa_10 == "B350 - Tinea barbae and tinea capitis" ~ NA,
      Diagnosa_10 == "B351 - Tinea unguium" ~ NA,
      Diagnosa_10 == "B352 - Tinea manuum" ~ NA,
      Diagnosa_10 == "B353 - Tinea pedis" ~ NA,
      Diagnosa_10 == "B354 - Tinea corporis" ~ NA,
      Diagnosa_10 == "B356 - Tinea cruris" ~ NA,
      Diagnosa_10 == "B359 - Dermatophytosis, unspecified" ~ NA,
      Diagnosa_10 == "B360 - Pityriasis versicolor" ~ NA,
      Diagnosa_10 == "B500 - Plasmodium falciparum malaria with cerebral complications" ~ NA,
      Diagnosa_10 == "B510 - Plasmodium vivax malaria with rupture of spleen" ~ NA,
      Diagnosa_10 == "B520 - Plasmodium malariae malaria with nephropathy" ~ NA,
      Diagnosa_10 == "B530 - Plasmodium ovale malaria" ~ NA,
      Diagnosa_10 == "B54 - Unspecified malaria" ~ NA,
      Diagnosa_10 == "B650 - Schistosomiasis due to Schistosoma haematobium [urinary schistosomiasis]" ~ NA,
      Diagnosa_10 == "B680 - Von Willebrand disease" ~ NA,
      Diagnosa_10 == "B740 - Filariasis due to Wuchereria bancrofti" ~ NA,
      Diagnosa_10 == "B760 - Ancylostomiasis" ~ NA,
      Diagnosa_10 == "B769 - Hookworm disease, unspecified" ~ NA,
      Diagnosa_10 == "B770 - Ascariasis with intestinal complications" ~ NA,
      Diagnosa_10 == "B780 - Intestinal strongyloidiasis" ~ NA,
      Diagnosa_10 == "B850 - Pediculosis due to Pediculus humanus capitis" ~ NA,
      Diagnosa_10 == "B853 - Phthiriasis" ~ NA,
      Diagnosa_10 == "B86 - Scabies" ~ NA,
      Diagnosa_10 == "D179 - Benign lipomatous neoplasm, unspecified" ~ NA,
      Diagnosa_10 == "D500 - Iron deficiency anaemia secondary to blood loss (chronic)" ~ NA,
      Diagnosa_10 == "D649 - Anaemia, unspecified" ~ NA,
      Diagnosa_10 == "E100 - Insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_10 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_10 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ NA,
      Diagnosa_10 == "E162 - Hypoglycaemia, unspecified" ~ NA,
      Diagnosa_10 == "E46 - Unspecified protein-energy malnutrition" ~ NA,
      Diagnosa_10 == "E569 - Vitamin deficiency, unspecified" ~ NA,
      Diagnosa_10 == "E610 - Copper deficiency" ~ NA,
      Diagnosa_10 == "E660 - Obesity due to excess calories" ~ NA,
      Diagnosa_10 == "E740 - Glycogen storage disease" ~ NA,
      Diagnosa_10 == "E785 - Hyperlipidaemia, unspecified" ~ NA,
      Diagnosa_10 == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease" ~ NA,
      Diagnosa_10 == "F450 - Somatization disorder" ~ NA,
      Diagnosa_10 == "G430 - Migraine without aura [common migraine]" ~ NA,
      Diagnosa_10 == "G442 - Tension-type headache" ~ NA,
      Diagnosa_10 == "G470 - Disorders of initiating and maintaining sleep [insomnias]" ~ NA,
      Diagnosa_10 == "G510 - Bell's palsy" ~ NA,
      Diagnosa_10 == "H000 - Hordeolum and other deep inflammation of eyelid" ~ NA,
      Diagnosa_10 == "H010 - Blepharitis" ~ NA,
      Diagnosa_10 == "H020 - Entropion and trichiasis of eyelid" ~ NA,
      Diagnosa_10 == "H100 - Mucopurulent conjunctivitis" ~ NA,
      Diagnosa_10 == "H113 - Conjunctival haemorrhage" ~ NA,
      Diagnosa_10 == "H151 - Episcleritis" ~ NA,
      Diagnosa_10 == "H162 - Keratoconjunctivitis" ~ NA,
      Diagnosa_10 == "H524 - Presbyopia" ~ NA,
      Diagnosa_10 == "H531 - Subjective visual disturbances" ~ NA,
      Diagnosa_10 == "H600 - Abscess of external ear" ~ NA,
      Diagnosa_10 == "H612 - Impacted cerumen" ~ NA,
      Diagnosa_10 == "H669 - Otitis media, unspecified" ~ NA,
      Diagnosa_10 == "H810 - Meniere's disease" ~ NA,
      Diagnosa_10 == "I10 - Essential (primary) hypertension" ~ NA,
      Diagnosa_10 == "I839 - Varicose veins of lower extremities without ulcer or inflammation" ~ NA,
      Diagnosa_10 == "I840 - Internal thrombosed haemorrhoids" ~ NA,
      Diagnosa_10 == "I880 - Nonspecific mesenteric lymphadenitis" ~ NA,
      Diagnosa_10 == "J00 - Acute nasopharyngitis [common cold]" ~ NA,
      Diagnosa_10 == "J020 - Streptococcal pharyngitis" ~ NA,
      Diagnosa_10 == "J030 - Streptococcal tonsillitis" ~ NA,
      Diagnosa_10 == "J040 - Acute laryngitis" ~ NA,
      Diagnosa_10 == "J100 - Influenza with pneumonia, other influenza virus identified" ~ NA,
      Diagnosa_10 == "J120 - Adenoviral pneumonia" ~ NA,
      Diagnosa_10 == "J300 - Vasomotor rhinitis" ~ NA,
      Diagnosa_10 == "J304 - Allergic rhinitis, unspecified" ~ NA,
      Diagnosa_10 == "J340 - Abscess, furuncle and carbuncle of nose" ~ NA,
      Diagnosa_10 == "J40 - Bronchitis, not specified as acute or chronic" ~ NA,
      Diagnosa_10 == "J459 - Asthma, unspecified" ~ NA,
      Diagnosa_10 == "K210 - Gastro-oesophageal reflux disease with oesophagitis" ~ NA,
      Diagnosa_10 == "K291 - Other acute gastritis" ~ NA,
      Diagnosa_10 == "K297 - Gastritis, unspecified" ~ NA,
      Diagnosa_10 == "K30 - Dyspepsia" ~ NA,
      Diagnosa_10 == "K529 - Noninfective gastroenteritis and colitis, unspecified" ~ NA,
      Diagnosa_10 == "L010 - Impeti [any organism] [any site]" ~ NA,
      Diagnosa_10 == "L020 - Cutaneous abscess, furuncle and carbuncle of face" ~ NA,
      Diagnosa_10 == "L022 - Cutaneous abscess, furuncle and carbuncle of trunk" ~ NA,
      Diagnosa_10 == "L028 - Cutaneous abscess, furuncle and carbuncle of other sites" ~ NA,
      Diagnosa_10 == "L080 - Pyoderma" ~ NA,
      Diagnosa_10 == "L081 - Erythrasma" ~ NA,
      Diagnosa_10 == "L089 - Local infection of skin and subcutaneous tissue, unspecified" ~ NA,
      Diagnosa_10 == "L200 - Besnier's pruri" ~ NA,
      Diagnosa_10 == "L210 - Seborrhoea capitis" ~ NA,
      Diagnosa_10 == "L240 - Irritant contact dermatitis due to detergents" ~ NA,
      Diagnosa_10 == "L270 - Generalized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_10 == "L271 - Localized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_10 == "L272 - Dermatitis due to ingested food" ~ NA,
      Diagnosa_10 == "L300 - Nummular dermatitis" ~ NA,
      Diagnosa_10 == "L309 - Dermatitis, unspecified" ~ NA,
      Diagnosa_10 == "L42 - Pityriasis rosea" ~ NA,
      Diagnosa_10 == "L500 - Allergic urticaria" ~ NA,
      Diagnosa_10 == "L700 - Acne vulgaris" ~ NA,
      Diagnosa_10 == "L710 - Perioral dermatitis" ~ NA,
      Diagnosa_10 == "L732 - Hidradenitis suppurativa" ~ NA,
      Diagnosa_10 == "L743 - Miliaria, unspecified" ~ NA,
      Diagnosa_10 == "N12 - Tubulo-interstitial nephritis, not specified as acute or chronic" ~ NA,
      Diagnosa_10 == "N390 - Urinary tract infection, site not specified" ~ NA,
      Diagnosa_10 == "N47 - Redundant prepuce, phimosis and paraphimosis" ~ NA,
      Diagnosa_10 == "N508 - Other specified disorders of male genital organs" ~ NA,
      Diagnosa_10 == "N61 - Inflammatory disorders of breast" ~ NA,
      Diagnosa_10 == "N645 - Other signs and symptoms in breast" ~ NA,
      Diagnosa_10 == "N700 - Acute salpingitis and oophoritis" ~ NA,
      Diagnosa_10 == "N760 - Acute vaginitis" ~ NA,
      Diagnosa_10 == "N762 - Acute vulvitis" ~ NA,
      Diagnosa_10 == "N768 - Other specified inflammation of vagina and vulva" ~ NA,
      Diagnosa_10 == "O000 - Abdominal pregnancy" ~ NA,
      Diagnosa_10 == "O030 - Spontaneous abortion, incomplete, complicated by genital tract and pelvic infection" ~ NA,
      Diagnosa_10 == "O25 - Malnutrition in pregnancy" ~ NA,
      Diagnosa_10 == "O700 - First degree perineal laceration during delivery" ~ NA,
      Diagnosa_10 == "O701 - Second degree perineal laceration during delivery" ~ NA,
      Diagnosa_10 == "O702 - Third degree perineal laceration during delivery" ~ NA,
      Diagnosa_10 == "O703 - Fourth degree perineal laceration during delivery" ~ NA,
      Diagnosa_10 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_10 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_10 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_10 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_10 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_10 == "O921 - Cracked nipple associated with childbirth" ~ NA,
      Diagnosa_10 == "R040 - Epistaxis" ~ NA,
      Diagnosa_10 == "R560 - Febrile convulsions" ~ NA,
      Diagnosa_10 == "R51 - Headache" ~ NA,
      Diagnosa_10 == "T151 - Foreign body in conjunctival sac" ~ NA,
      Diagnosa_10 == "T171 - Foreign body in nostril" ~ NA,
      Diagnosa_10 == "T301 - Burn of first degree, body region unspecified" ~ NA,
      Diagnosa_10 == "T302 - Burn of second degree, body region unspecified" ~ NA,
      Diagnosa_10 == "T753 - Motion sickness" ~ NA,
      Diagnosa_10 == "T782 - Anaphylactic shock, unspecified" ~ NA,
      Diagnosa_10 == "T812 - Accidental puncture and laceration during a procedure, not elsewhere classified" ~ NA,
      Diagnosa_10 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_10 == "Z370 - Single live birth" ~ NA,
      TRUE ~ Diagnosa_10)) %>%
  select(-Diagnosa_10)

nonspes <- nonspes %>%
  mutate(
    Diagnosa_011 = case_when(
      Diagnosa_11 == "A010 - Typhoid fever" ~ NA,
      Diagnosa_11 == "A059 - Bacterial foodborne intoxication, unspecified" ~ NA,
      Diagnosa_11 == "A060 - Acute amoebic dysentery" ~ NA,
      Diagnosa_11 == "A150 - Tuberculosis of lung, confirmed by sputum microscopy with or without culture" ~ NA,
      Diagnosa_11 == "A184 - Tuberculosis of skin and subcutaneous tissue" ~ NA,
      Diagnosa_11 == "A270 - Leptospirosis icterohaemorrhagica" ~ NA,
      Diagnosa_11 == "A300 - Indeterminate leprosy" ~ NA,
      Diagnosa_11 == "A35 - Other tetanus" ~ NA,
      Diagnosa_11 == "A370 - Whooping cough due to Bordetella pertussis" ~ NA,
      Diagnosa_11 == "A46 - Erysipelas" ~ NA,
      Diagnosa_11 == "A510 - Primary genital syphilis" ~ NA,
      Diagnosa_11 == "A520 - Cardiovascular syphilis" ~ NA,
      Diagnosa_11 == "A540 - nococcal infection of lower genitourinary tract without periurethral or accessory gland abscess" ~ NA,
      Diagnosa_11 == "A90 - Dengue fever [classical dengue]" ~ NA,
      Diagnosa_11 == "A91 - Dengue haemorrhagic fever" ~ NA,
      Diagnosa_11 == "A920 - Chikungunya virus disease" ~ NA,
      Diagnosa_11 == "B001 - Herpesviral vesicular dermatitis" ~ NA,
      Diagnosa_11 == "B009 - Herpesviral infection, unspecified" ~ NA,
      Diagnosa_11 == "B019 - Varicella without complication" ~ NA,
      Diagnosa_11 == "B029 - Zoster without complication" ~ NA,
      Diagnosa_11 == "B059 - Measles without complication" ~ NA,
      Diagnosa_11 == "B07 - Viral warts" ~ NA,
      Diagnosa_11 == "B081 - Molluscum contagiosum" ~ NA,
      Diagnosa_11 == "B150 - Hepatitis A with hepatic coma" ~ NA,
      Diagnosa_11 == "B160 - Acute hepatitis B with delta-agent (coinfection) with hepatic coma" ~ NA,
      Diagnosa_11 == "B170 - Acute delta-(super)infection of hepatitis B carrier" ~ NA,
      Diagnosa_11 == "B171 - Acute hepatitis C" ~ NA,
      Diagnosa_11 == "B172 - Acute hepatitis E" ~ NA,
      Diagnosa_11 == "B24 - Unspecified human immunodeficiency virus [HIV] disease" ~ NA,
      Diagnosa_11 == "B260 - Mumps orchitis" ~ NA,
      Diagnosa_11 == "B350 - Tinea barbae and tinea capitis" ~ NA,
      Diagnosa_11 == "B351 - Tinea unguium" ~ NA,
      Diagnosa_11 == "B352 - Tinea manuum" ~ NA,
      Diagnosa_11 == "B353 - Tinea pedis" ~ NA,
      Diagnosa_11 == "B354 - Tinea corporis" ~ NA,
      Diagnosa_11 == "B356 - Tinea cruris" ~ NA,
      Diagnosa_11 == "B359 - Dermatophytosis, unspecified" ~ NA,
      Diagnosa_11 == "B360 - Pityriasis versicolor" ~ NA,
      Diagnosa_11 == "B500 - Plasmodium falciparum malaria with cerebral complications" ~ NA,
      Diagnosa_11 == "B510 - Plasmodium vivax malaria with rupture of spleen" ~ NA,
      Diagnosa_11 == "B520 - Plasmodium malariae malaria with nephropathy" ~ NA,
      Diagnosa_11 == "B530 - Plasmodium ovale malaria" ~ NA,
      Diagnosa_11 == "B54 - Unspecified malaria" ~ NA,
      Diagnosa_11 == "B650 - Schistosomiasis due to Schistosoma haematobium [urinary schistosomiasis]" ~ NA,
      Diagnosa_11 == "B680 - Von Willebrand disease" ~ NA,
      Diagnosa_11 == "B740 - Filariasis due to Wuchereria bancrofti" ~ NA,
      Diagnosa_11 == "B760 - Ancylostomiasis" ~ NA,
      Diagnosa_11 == "B769 - Hookworm disease, unspecified" ~ NA,
      Diagnosa_11 == "B770 - Ascariasis with intestinal complications" ~ NA,
      Diagnosa_11 == "B780 - Intestinal strongyloidiasis" ~ NA,
      Diagnosa_11 == "B850 - Pediculosis due to Pediculus humanus capitis" ~ NA,
      Diagnosa_11 == "B853 - Phthiriasis" ~ NA,
      Diagnosa_11 == "B86 - Scabies" ~ NA,
      Diagnosa_11 == "D179 - Benign lipomatous neoplasm, unspecified" ~ NA,
      Diagnosa_11 == "D500 - Iron deficiency anaemia secondary to blood loss (chronic)" ~ NA,
      Diagnosa_11 == "D649 - Anaemia, unspecified" ~ NA,
      Diagnosa_11 == "E100 - Insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_11 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_11 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ NA,
      Diagnosa_11 == "E162 - Hypoglycaemia, unspecified" ~ NA,
      Diagnosa_11 == "E46 - Unspecified protein-energy malnutrition" ~ NA,
      Diagnosa_11 == "E569 - Vitamin deficiency, unspecified" ~ NA,
      Diagnosa_11 == "E610 - Copper deficiency" ~ NA,
      Diagnosa_11 == "E660 - Obesity due to excess calories" ~ NA,
      Diagnosa_11 == "E740 - Glycogen storage disease" ~ NA,
      Diagnosa_11 == "E785 - Hyperlipidaemia, unspecified" ~ NA,
      Diagnosa_11 == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease" ~ NA,
      Diagnosa_11 == "F450 - Somatization disorder" ~ NA,
      Diagnosa_11 == "G430 - Migraine without aura [common migraine]" ~ NA,
      Diagnosa_11 == "G442 - Tension-type headache" ~ NA,
      Diagnosa_11 == "G470 - Disorders of initiating and maintaining sleep [insomnias]" ~ NA,
      Diagnosa_11 == "G510 - Bell's palsy" ~ NA,
      Diagnosa_11 == "H000 - Hordeolum and other deep inflammation of eyelid" ~ NA,
      Diagnosa_11 == "H010 - Blepharitis" ~ NA,
      Diagnosa_11 == "H020 - Entropion and trichiasis of eyelid" ~ NA,
      Diagnosa_11 == "H100 - Mucopurulent conjunctivitis" ~ NA,
      Diagnosa_11 == "H113 - Conjunctival haemorrhage" ~ NA,
      Diagnosa_11 == "H151 - Episcleritis" ~ NA,
      Diagnosa_11 == "H162 - Keratoconjunctivitis" ~ NA,
      Diagnosa_11 == "H524 - Presbyopia" ~ NA,
      Diagnosa_11 == "H531 - Subjective visual disturbances" ~ NA,
      Diagnosa_11 == "H600 - Abscess of external ear" ~ NA,
      Diagnosa_11 == "H612 - Impacted cerumen" ~ NA,
      Diagnosa_11 == "H669 - Otitis media, unspecified" ~ NA,
      Diagnosa_11 == "H810 - Meniere's disease" ~ NA,
      Diagnosa_11 == "I10 - Essential (primary) hypertension" ~ NA,
      Diagnosa_11 == "I839 - Varicose veins of lower extremities without ulcer or inflammation" ~ NA,
      Diagnosa_11 == "I840 - Internal thrombosed haemorrhoids" ~ NA,
      Diagnosa_11 == "I880 - Nonspecific mesenteric lymphadenitis" ~ NA,
      Diagnosa_11 == "J00 - Acute nasopharyngitis [common cold]" ~ NA,
      Diagnosa_11 == "J020 - Streptococcal pharyngitis" ~ NA,
      Diagnosa_11 == "J030 - Streptococcal tonsillitis" ~ NA,
      Diagnosa_11 == "J040 - Acute laryngitis" ~ NA,
      Diagnosa_11 == "J100 - Influenza with pneumonia, other influenza virus identified" ~ NA,
      Diagnosa_11 == "J120 - Adenoviral pneumonia" ~ NA,
      Diagnosa_11 == "J300 - Vasomotor rhinitis" ~ NA,
      Diagnosa_11 == "J304 - Allergic rhinitis, unspecified" ~ NA,
      Diagnosa_11 == "J340 - Abscess, furuncle and carbuncle of nose" ~ NA,
      Diagnosa_11 == "J40 - Bronchitis, not specified as acute or chronic" ~ NA,
      Diagnosa_11 == "J459 - Asthma, unspecified" ~ NA,
      Diagnosa_11 == "K210 - Gastro-oesophageal reflux disease with oesophagitis" ~ NA,
      Diagnosa_11 == "K291 - Other acute gastritis" ~ NA,
      Diagnosa_11 == "K297 - Gastritis, unspecified" ~ NA,
      Diagnosa_11 == "K30 - Dyspepsia" ~ NA,
      Diagnosa_11 == "K529 - Noninfective gastroenteritis and colitis, unspecified" ~ NA,
      Diagnosa_11 == "L010 - Impeti [any organism] [any site]" ~ NA,
      Diagnosa_11 == "L020 - Cutaneous abscess, furuncle and carbuncle of face" ~ NA,
      Diagnosa_11 == "L022 - Cutaneous abscess, furuncle and carbuncle of trunk" ~ NA,
      Diagnosa_11 == "L028 - Cutaneous abscess, furuncle and carbuncle of other sites" ~ NA,
      Diagnosa_11 == "L080 - Pyoderma" ~ NA,
      Diagnosa_11 == "L081 - Erythrasma" ~ NA,
      Diagnosa_11 == "L089 - Local infection of skin and subcutaneous tissue, unspecified" ~ NA,
      Diagnosa_11 == "L200 - Besnier's pruri" ~ NA,
      Diagnosa_11 == "L210 - Seborrhoea capitis" ~ NA,
      Diagnosa_11 == "L240 - Irritant contact dermatitis due to detergents" ~ NA,
      Diagnosa_11 == "L270 - Generalized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_11 == "L271 - Localized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_11 == "L272 - Dermatitis due to ingested food" ~ NA,
      Diagnosa_11 == "L300 - Nummular dermatitis" ~ NA,
      Diagnosa_11 == "L309 - Dermatitis, unspecified" ~ NA,
      Diagnosa_11 == "L42 - Pityriasis rosea" ~ NA,
      Diagnosa_11 == "L500 - Allergic urticaria" ~ NA,
      Diagnosa_11 == "L700 - Acne vulgaris" ~ NA,
      Diagnosa_11 == "L710 - Perioral dermatitis" ~ NA,
      Diagnosa_11 == "L732 - Hidradenitis suppurativa" ~ NA,
      Diagnosa_11 == "L743 - Miliaria, unspecified" ~ NA,
      Diagnosa_11 == "N12 - Tubulo-interstitial nephritis, not specified as acute or chronic" ~ NA,
      Diagnosa_11 == "N390 - Urinary tract infection, site not specified" ~ NA,
      Diagnosa_11 == "N47 - Redundant prepuce, phimosis and paraphimosis" ~ NA,
      Diagnosa_11 == "N508 - Other specified disorders of male genital organs" ~ NA,
      Diagnosa_11 == "N61 - Inflammatory disorders of breast" ~ NA,
      Diagnosa_11 == "N645 - Other signs and symptoms in breast" ~ NA,
      Diagnosa_11 == "N700 - Acute salpingitis and oophoritis" ~ NA,
      Diagnosa_11 == "N760 - Acute vaginitis" ~ NA,
      Diagnosa_11 == "N762 - Acute vulvitis" ~ NA,
      Diagnosa_11 == "N768 - Other specified inflammation of vagina and vulva" ~ NA,
      Diagnosa_11 == "O000 - Abdominal pregnancy" ~ NA,
      Diagnosa_11 == "O030 - Spontaneous abortion, incomplete, complicated by genital tract and pelvic infection" ~ NA,
      Diagnosa_11 == "O25 - Malnutrition in pregnancy" ~ NA,
      Diagnosa_11 == "O700 - First degree perineal laceration during delivery" ~ NA,
      Diagnosa_11 == "O701 - Second degree perineal laceration during delivery" ~ NA,
      Diagnosa_11 == "O702 - Third degree perineal laceration during delivery" ~ NA,
      Diagnosa_11 == "O703 - Fourth degree perineal laceration during delivery" ~ NA,
      Diagnosa_11 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_11 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_11 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_11 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_11 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_11 == "O921 - Cracked nipple associated with childbirth" ~ NA,
      Diagnosa_11 == "R040 - Epistaxis" ~ NA,
      Diagnosa_11 == "R560 - Febrile convulsions" ~ NA,
      Diagnosa_11 == "R51 - Headache" ~ NA,
      Diagnosa_11 == "T151 - Foreign body in conjunctival sac" ~ NA,
      Diagnosa_11 == "T171 - Foreign body in nostril" ~ NA,
      Diagnosa_11 == "T301 - Burn of first degree, body region unspecified" ~ NA,
      Diagnosa_11 == "T302 - Burn of second degree, body region unspecified" ~ NA,
      Diagnosa_11 == "T753 - Motion sickness" ~ NA,
      Diagnosa_11 == "T782 - Anaphylactic shock, unspecified" ~ NA,
      Diagnosa_11 == "T812 - Accidental puncture and laceration during a procedure, not elsewhere classified" ~ NA,
      Diagnosa_11 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_11 == "Z370 - Single live birth" ~ NA,
      TRUE ~ Diagnosa_11)) %>%
  select(-Diagnosa_11)

nonspes <- nonspes %>%
  mutate(
    Diagnosa_012 = case_when(
      Diagnosa_12 == "A010 - Typhoid fever" ~ NA,
      Diagnosa_12 == "A059 - Bacterial foodborne intoxication, unspecified" ~ NA,
      Diagnosa_12 == "A060 - Acute amoebic dysentery" ~ NA,
      Diagnosa_12 == "A150 - Tuberculosis of lung, confirmed by sputum microscopy with or without culture" ~ NA,
      Diagnosa_12 == "A184 - Tuberculosis of skin and subcutaneous tissue" ~ NA,
      Diagnosa_12 == "A270 - Leptospirosis icterohaemorrhagica" ~ NA,
      Diagnosa_12 == "A300 - Indeterminate leprosy" ~ NA,
      Diagnosa_12 == "A35 - Other tetanus" ~ NA,
      Diagnosa_12 == "A370 - Whooping cough due to Bordetella pertussis" ~ NA,
      Diagnosa_12 == "A46 - Erysipelas" ~ NA,
      Diagnosa_12 == "A510 - Primary genital syphilis" ~ NA,
      Diagnosa_12 == "A520 - Cardiovascular syphilis" ~ NA,
      Diagnosa_12 == "A540 - nococcal infection of lower genitourinary tract without periurethral or accessory gland abscess" ~ NA,
      Diagnosa_12 == "A90 - Dengue fever [classical dengue]" ~ NA,
      Diagnosa_12 == "A91 - Dengue haemorrhagic fever" ~ NA,
      Diagnosa_12 == "A920 - Chikungunya virus disease" ~ NA,
      Diagnosa_12 == "B001 - Herpesviral vesicular dermatitis" ~ NA,
      Diagnosa_12 == "B009 - Herpesviral infection, unspecified" ~ NA,
      Diagnosa_12 == "B019 - Varicella without complication" ~ NA,
      Diagnosa_12 == "B029 - Zoster without complication" ~ NA,
      Diagnosa_12 == "B059 - Measles without complication" ~ NA,
      Diagnosa_12 == "B07 - Viral warts" ~ NA,
      Diagnosa_12 == "B081 - Molluscum contagiosum" ~ NA,
      Diagnosa_12 == "B150 - Hepatitis A with hepatic coma" ~ NA,
      Diagnosa_12 == "B160 - Acute hepatitis B with delta-agent (coinfection) with hepatic coma" ~ NA,
      Diagnosa_12 == "B170 - Acute delta-(super)infection of hepatitis B carrier" ~ NA,
      Diagnosa_12 == "B171 - Acute hepatitis C" ~ NA,
      Diagnosa_12 == "B172 - Acute hepatitis E" ~ NA,
      Diagnosa_12 == "B24 - Unspecified human immunodeficiency virus [HIV] disease" ~ NA,
      Diagnosa_12 == "B260 - Mumps orchitis" ~ NA,
      Diagnosa_12 == "B350 - Tinea barbae and tinea capitis" ~ NA,
      Diagnosa_12 == "B351 - Tinea unguium" ~ NA,
      Diagnosa_12 == "B352 - Tinea manuum" ~ NA,
      Diagnosa_12 == "B353 - Tinea pedis" ~ NA,
      Diagnosa_12 == "B354 - Tinea corporis" ~ NA,
      Diagnosa_12 == "B356 - Tinea cruris" ~ NA,
      Diagnosa_12 == "B359 - Dermatophytosis, unspecified" ~ NA,
      Diagnosa_12 == "B360 - Pityriasis versicolor" ~ NA,
      Diagnosa_12 == "B500 - Plasmodium falciparum malaria with cerebral complications" ~ NA,
      Diagnosa_12 == "B510 - Plasmodium vivax malaria with rupture of spleen" ~ NA,
      Diagnosa_12 == "B520 - Plasmodium malariae malaria with nephropathy" ~ NA,
      Diagnosa_12 == "B530 - Plasmodium ovale malaria" ~ NA,
      Diagnosa_12 == "B54 - Unspecified malaria" ~ NA,
      Diagnosa_12 == "B650 - Schistosomiasis due to Schistosoma haematobium [urinary schistosomiasis]" ~ NA,
      Diagnosa_12 == "B680 - Von Willebrand disease" ~ NA,
      Diagnosa_12 == "B740 - Filariasis due to Wuchereria bancrofti" ~ NA,
      Diagnosa_12 == "B760 - Ancylostomiasis" ~ NA,
      Diagnosa_12 == "B769 - Hookworm disease, unspecified" ~ NA,
      Diagnosa_12 == "B770 - Ascariasis with intestinal complications" ~ NA,
      Diagnosa_12 == "B780 - Intestinal strongyloidiasis" ~ NA,
      Diagnosa_12 == "B850 - Pediculosis due to Pediculus humanus capitis" ~ NA,
      Diagnosa_12 == "B853 - Phthiriasis" ~ NA,
      Diagnosa_12 == "B86 - Scabies" ~ NA,
      Diagnosa_12 == "D179 - Benign lipomatous neoplasm, unspecified" ~ NA,
      Diagnosa_12 == "D500 - Iron deficiency anaemia secondary to blood loss (chronic)" ~ NA,
      Diagnosa_12 == "D649 - Anaemia, unspecified" ~ NA,
      Diagnosa_12 == "E100 - Insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_12 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_12 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ NA,
      Diagnosa_12 == "E162 - Hypoglycaemia, unspecified" ~ NA,
      Diagnosa_12 == "E46 - Unspecified protein-energy malnutrition" ~ NA,
      Diagnosa_12 == "E569 - Vitamin deficiency, unspecified" ~ NA,
      Diagnosa_12 == "E610 - Copper deficiency" ~ NA,
      Diagnosa_12 == "E660 - Obesity due to excess calories" ~ NA,
      Diagnosa_12 == "E740 - Glycogen storage disease" ~ NA,
      Diagnosa_12 == "E785 - Hyperlipidaemia, unspecified" ~ NA,
      Diagnosa_12 == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease" ~ NA,
      Diagnosa_12 == "F450 - Somatization disorder" ~ NA,
      Diagnosa_12 == "G430 - Migraine without aura [common migraine]" ~ NA,
      Diagnosa_12 == "G442 - Tension-type headache" ~ NA,
      Diagnosa_12 == "G470 - Disorders of initiating and maintaining sleep [insomnias]" ~ NA,
      Diagnosa_12 == "G510 - Bell's palsy" ~ NA,
      Diagnosa_12 == "H000 - Hordeolum and other deep inflammation of eyelid" ~ NA,
      Diagnosa_12 == "H010 - Blepharitis" ~ NA,
      Diagnosa_12 == "H020 - Entropion and trichiasis of eyelid" ~ NA,
      Diagnosa_12 == "H100 - Mucopurulent conjunctivitis" ~ NA,
      Diagnosa_12 == "H113 - Conjunctival haemorrhage" ~ NA,
      Diagnosa_12 == "H151 - Episcleritis" ~ NA,
      Diagnosa_12 == "H162 - Keratoconjunctivitis" ~ NA,
      Diagnosa_12 == "H524 - Presbyopia" ~ NA,
      Diagnosa_12 == "H531 - Subjective visual disturbances" ~ NA,
      Diagnosa_12 == "H600 - Abscess of external ear" ~ NA,
      Diagnosa_12 == "H612 - Impacted cerumen" ~ NA,
      Diagnosa_12 == "H669 - Otitis media, unspecified" ~ NA,
      Diagnosa_12 == "H810 - Meniere's disease" ~ NA,
      Diagnosa_12 == "I10 - Essential (primary) hypertension" ~ NA,
      Diagnosa_12 == "I839 - Varicose veins of lower extremities without ulcer or inflammation" ~ NA,
      Diagnosa_12 == "I840 - Internal thrombosed haemorrhoids" ~ NA,
      Diagnosa_12 == "I880 - Nonspecific mesenteric lymphadenitis" ~ NA,
      Diagnosa_12 == "J00 - Acute nasopharyngitis [common cold]" ~ NA,
      Diagnosa_12 == "J020 - Streptococcal pharyngitis" ~ NA,
      Diagnosa_12 == "J030 - Streptococcal tonsillitis" ~ NA,
      Diagnosa_12 == "J040 - Acute laryngitis" ~ NA,
      Diagnosa_12 == "J100 - Influenza with pneumonia, other influenza virus identified" ~ NA,
      Diagnosa_12 == "J120 - Adenoviral pneumonia" ~ NA,
      Diagnosa_12 == "J300 - Vasomotor rhinitis" ~ NA,
      Diagnosa_12 == "J304 - Allergic rhinitis, unspecified" ~ NA,
      Diagnosa_12 == "J340 - Abscess, furuncle and carbuncle of nose" ~ NA,
      Diagnosa_12 == "J40 - Bronchitis, not specified as acute or chronic" ~ NA,
      Diagnosa_12 == "J459 - Asthma, unspecified" ~ NA,
      Diagnosa_12 == "K210 - Gastro-oesophageal reflux disease with oesophagitis" ~ NA,
      Diagnosa_12 == "K291 - Other acute gastritis" ~ NA,
      Diagnosa_12 == "K297 - Gastritis, unspecified" ~ NA,
      Diagnosa_12 == "K30 - Dyspepsia" ~ NA,
      Diagnosa_12 == "K529 - Noninfective gastroenteritis and colitis, unspecified" ~ NA,
      Diagnosa_12 == "L010 - Impeti [any organism] [any site]" ~ NA,
      Diagnosa_12 == "L020 - Cutaneous abscess, furuncle and carbuncle of face" ~ NA,
      Diagnosa_12 == "L022 - Cutaneous abscess, furuncle and carbuncle of trunk" ~ NA,
      Diagnosa_12 == "L028 - Cutaneous abscess, furuncle and carbuncle of other sites" ~ NA,
      Diagnosa_12 == "L080 - Pyoderma" ~ NA,
      Diagnosa_12 == "L081 - Erythrasma" ~ NA,
      Diagnosa_12 == "L089 - Local infection of skin and subcutaneous tissue, unspecified" ~ NA,
      Diagnosa_12 == "L200 - Besnier's pruri" ~ NA,
      Diagnosa_12 == "L210 - Seborrhoea capitis" ~ NA,
      Diagnosa_12 == "L240 - Irritant contact dermatitis due to detergents" ~ NA,
      Diagnosa_12 == "L270 - Generalized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_12 == "L271 - Localized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_12 == "L272 - Dermatitis due to ingested food" ~ NA,
      Diagnosa_12 == "L300 - Nummular dermatitis" ~ NA,
      Diagnosa_12 == "L309 - Dermatitis, unspecified" ~ NA,
      Diagnosa_12 == "L42 - Pityriasis rosea" ~ NA,
      Diagnosa_12 == "L500 - Allergic urticaria" ~ NA,
      Diagnosa_12 == "L700 - Acne vulgaris" ~ NA,
      Diagnosa_12 == "L710 - Perioral dermatitis" ~ NA,
      Diagnosa_12 == "L732 - Hidradenitis suppurativa" ~ NA,
      Diagnosa_12 == "L743 - Miliaria, unspecified" ~ NA,
      Diagnosa_12 == "N12 - Tubulo-interstitial nephritis, not specified as acute or chronic" ~ NA,
      Diagnosa_12 == "N390 - Urinary tract infection, site not specified" ~ NA,
      Diagnosa_12 == "N47 - Redundant prepuce, phimosis and paraphimosis" ~ NA,
      Diagnosa_12 == "N508 - Other specified disorders of male genital organs" ~ NA,
      Diagnosa_12 == "N61 - Inflammatory disorders of breast" ~ NA,
      Diagnosa_12 == "N645 - Other signs and symptoms in breast" ~ NA,
      Diagnosa_12 == "N700 - Acute salpingitis and oophoritis" ~ NA,
      Diagnosa_12 == "N760 - Acute vaginitis" ~ NA,
      Diagnosa_12 == "N762 - Acute vulvitis" ~ NA,
      Diagnosa_12 == "N768 - Other specified inflammation of vagina and vulva" ~ NA,
      Diagnosa_12 == "O000 - Abdominal pregnancy" ~ NA,
      Diagnosa_12 == "O030 - Spontaneous abortion, incomplete, complicated by genital tract and pelvic infection" ~ NA,
      Diagnosa_12 == "O25 - Malnutrition in pregnancy" ~ NA,
      Diagnosa_12 == "O700 - First degree perineal laceration during delivery" ~ NA,
      Diagnosa_12 == "O701 - Second degree perineal laceration during delivery" ~ NA,
      Diagnosa_12 == "O702 - Third degree perineal laceration during delivery" ~ NA,
      Diagnosa_12 == "O703 - Fourth degree perineal laceration during delivery" ~ NA,
      Diagnosa_12 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_12 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_12 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_12 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_12 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_12 == "O921 - Cracked nipple associated with childbirth" ~ NA,
      Diagnosa_12 == "R040 - Epistaxis" ~ NA,
      Diagnosa_12 == "R560 - Febrile convulsions" ~ NA,
      Diagnosa_12 == "R51 - Headache" ~ NA,
      Diagnosa_12 == "T151 - Foreign body in conjunctival sac" ~ NA,
      Diagnosa_12 == "T171 - Foreign body in nostril" ~ NA,
      Diagnosa_12 == "T301 - Burn of first degree, body region unspecified" ~ NA,
      Diagnosa_12 == "T302 - Burn of second degree, body region unspecified" ~ NA,
      Diagnosa_12 == "T753 - Motion sickness" ~ NA,
      Diagnosa_12 == "T782 - Anaphylactic shock, unspecified" ~ NA,
      Diagnosa_12 == "T812 - Accidental puncture and laceration during a procedure, not elsewhere classified" ~ NA,
      Diagnosa_12 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_12 == "Z370 - Single live birth" ~ NA,
      TRUE ~ Diagnosa_12)) %>%
  select(-Diagnosa_12)

nonspes <- nonspes %>%
  mutate(
    Diagnosa_013 = case_when(
      Diagnosa_13 == "A010 - Typhoid fever" ~ NA,
      Diagnosa_13 == "A059 - Bacterial foodborne intoxication, unspecified" ~ NA,
      Diagnosa_13 == "A060 - Acute amoebic dysentery" ~ NA,
      Diagnosa_13 == "A150 - Tuberculosis of lung, confirmed by sputum microscopy with or without culture" ~ NA,
      Diagnosa_13 == "A184 - Tuberculosis of skin and subcutaneous tissue" ~ NA,
      Diagnosa_13 == "A270 - Leptospirosis icterohaemorrhagica" ~ NA,
      Diagnosa_13 == "A300 - Indeterminate leprosy" ~ NA,
      Diagnosa_13 == "A35 - Other tetanus" ~ NA,
      Diagnosa_13 == "A370 - Whooping cough due to Bordetella pertussis" ~ NA,
      Diagnosa_13 == "A46 - Erysipelas" ~ NA,
      Diagnosa_13 == "A510 - Primary genital syphilis" ~ NA,
      Diagnosa_13 == "A520 - Cardiovascular syphilis" ~ NA,
      Diagnosa_13 == "A540 - nococcal infection of lower genitourinary tract without periurethral or accessory gland abscess" ~ NA,
      Diagnosa_13 == "A90 - Dengue fever [classical dengue]" ~ NA,
      Diagnosa_13 == "A91 - Dengue haemorrhagic fever" ~ NA,
      Diagnosa_13 == "A920 - Chikungunya virus disease" ~ NA,
      Diagnosa_13 == "B001 - Herpesviral vesicular dermatitis" ~ NA,
      Diagnosa_13 == "B009 - Herpesviral infection, unspecified" ~ NA,
      Diagnosa_13 == "B019 - Varicella without complication" ~ NA,
      Diagnosa_13 == "B029 - Zoster without complication" ~ NA,
      Diagnosa_13 == "B059 - Measles without complication" ~ NA,
      Diagnosa_13 == "B07 - Viral warts" ~ NA,
      Diagnosa_13 == "B081 - Molluscum contagiosum" ~ NA,
      Diagnosa_13 == "B150 - Hepatitis A with hepatic coma" ~ NA,
      Diagnosa_13 == "B160 - Acute hepatitis B with delta-agent (coinfection) with hepatic coma" ~ NA,
      Diagnosa_13 == "B170 - Acute delta-(super)infection of hepatitis B carrier" ~ NA,
      Diagnosa_13 == "B171 - Acute hepatitis C" ~ NA,
      Diagnosa_13 == "B172 - Acute hepatitis E" ~ NA,
      Diagnosa_13 == "B24 - Unspecified human immunodeficiency virus [HIV] disease" ~ NA,
      Diagnosa_13 == "B260 - Mumps orchitis" ~ NA,
      Diagnosa_13 == "B350 - Tinea barbae and tinea capitis" ~ NA,
      Diagnosa_13 == "B351 - Tinea unguium" ~ NA,
      Diagnosa_13 == "B352 - Tinea manuum" ~ NA,
      Diagnosa_13 == "B353 - Tinea pedis" ~ NA,
      Diagnosa_13 == "B354 - Tinea corporis" ~ NA,
      Diagnosa_13 == "B356 - Tinea cruris" ~ NA,
      Diagnosa_13 == "B359 - Dermatophytosis, unspecified" ~ NA,
      Diagnosa_13 == "B360 - Pityriasis versicolor" ~ NA,
      Diagnosa_13 == "B500 - Plasmodium falciparum malaria with cerebral complications" ~ NA,
      Diagnosa_13 == "B510 - Plasmodium vivax malaria with rupture of spleen" ~ NA,
      Diagnosa_13 == "B520 - Plasmodium malariae malaria with nephropathy" ~ NA,
      Diagnosa_13 == "B530 - Plasmodium ovale malaria" ~ NA,
      Diagnosa_13 == "B54 - Unspecified malaria" ~ NA,
      Diagnosa_13 == "B650 - Schistosomiasis due to Schistosoma haematobium [urinary schistosomiasis]" ~ NA,
      Diagnosa_13 == "B680 - Von Willebrand disease" ~ NA,
      Diagnosa_13 == "B740 - Filariasis due to Wuchereria bancrofti" ~ NA,
      Diagnosa_13 == "B760 - Ancylostomiasis" ~ NA,
      Diagnosa_13 == "B769 - Hookworm disease, unspecified" ~ NA,
      Diagnosa_13 == "B770 - Ascariasis with intestinal complications" ~ NA,
      Diagnosa_13 == "B780 - Intestinal strongyloidiasis" ~ NA,
      Diagnosa_13 == "B850 - Pediculosis due to Pediculus humanus capitis" ~ NA,
      Diagnosa_13 == "B853 - Phthiriasis" ~ NA,
      Diagnosa_13 == "B86 - Scabies" ~ NA,
      Diagnosa_13 == "D179 - Benign lipomatous neoplasm, unspecified" ~ NA,
      Diagnosa_13 == "D500 - Iron deficiency anaemia secondary to blood loss (chronic)" ~ NA,
      Diagnosa_13 == "D649 - Anaemia, unspecified" ~ NA,
      Diagnosa_13 == "E100 - Insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_13 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ NA,
      Diagnosa_13 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ NA,
      Diagnosa_13 == "E162 - Hypoglycaemia, unspecified" ~ NA,
      Diagnosa_13 == "E46 - Unspecified protein-energy malnutrition" ~ NA,
      Diagnosa_13 == "E569 - Vitamin deficiency, unspecified" ~ NA,
      Diagnosa_13 == "E610 - Copper deficiency" ~ NA,
      Diagnosa_13 == "E660 - Obesity due to excess calories" ~ NA,
      Diagnosa_13 == "E740 - Glycogen storage disease" ~ NA,
      Diagnosa_13 == "E785 - Hyperlipidaemia, unspecified" ~ NA,
      Diagnosa_13 == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease" ~ NA,
      Diagnosa_13 == "F450 - Somatization disorder" ~ NA,
      Diagnosa_13 == "G430 - Migraine without aura [common migraine]" ~ NA,
      Diagnosa_13 == "G442 - Tension-type headache" ~ NA,
      Diagnosa_13 == "G470 - Disorders of initiating and maintaining sleep [insomnias]" ~ NA,
      Diagnosa_13 == "G510 - Bell's palsy" ~ NA,
      Diagnosa_13 == "H000 - Hordeolum and other deep inflammation of eyelid" ~ NA,
      Diagnosa_13 == "H010 - Blepharitis" ~ NA,
      Diagnosa_13 == "H020 - Entropion and trichiasis of eyelid" ~ NA,
      Diagnosa_13 == "H100 - Mucopurulent conjunctivitis" ~ NA,
      Diagnosa_13 == "H113 - Conjunctival haemorrhage" ~ NA,
      Diagnosa_13 == "H151 - Episcleritis" ~ NA,
      Diagnosa_13 == "H162 - Keratoconjunctivitis" ~ NA,
      Diagnosa_13 == "H524 - Presbyopia" ~ NA,
      Diagnosa_13 == "H531 - Subjective visual disturbances" ~ NA,
      Diagnosa_13 == "H600 - Abscess of external ear" ~ NA,
      Diagnosa_13 == "H612 - Impacted cerumen" ~ NA,
      Diagnosa_13 == "H669 - Otitis media, unspecified" ~ NA,
      Diagnosa_13 == "H810 - Meniere's disease" ~ NA,
      Diagnosa_13 == "I10 - Essential (primary) hypertension" ~ NA,
      Diagnosa_13 == "I839 - Varicose veins of lower extremities without ulcer or inflammation" ~ NA,
      Diagnosa_13 == "I840 - Internal thrombosed haemorrhoids" ~ NA,
      Diagnosa_13 == "I880 - Nonspecific mesenteric lymphadenitis" ~ NA,
      Diagnosa_13 == "J00 - Acute nasopharyngitis [common cold]" ~ NA,
      Diagnosa_13 == "J020 - Streptococcal pharyngitis" ~ NA,
      Diagnosa_13 == "J030 - Streptococcal tonsillitis" ~ NA,
      Diagnosa_13 == "J040 - Acute laryngitis" ~ NA,
      Diagnosa_13 == "J100 - Influenza with pneumonia, other influenza virus identified" ~ NA,
      Diagnosa_13 == "J120 - Adenoviral pneumonia" ~ NA,
      Diagnosa_13 == "J300 - Vasomotor rhinitis" ~ NA,
      Diagnosa_13 == "J304 - Allergic rhinitis, unspecified" ~ NA,
      Diagnosa_13 == "J340 - Abscess, furuncle and carbuncle of nose" ~ NA,
      Diagnosa_13 == "J40 - Bronchitis, not specified as acute or chronic" ~ NA,
      Diagnosa_13 == "J459 - Asthma, unspecified" ~ NA,
      Diagnosa_13 == "K210 - Gastro-oesophageal reflux disease with oesophagitis" ~ NA,
      Diagnosa_13 == "K291 - Other acute gastritis" ~ NA,
      Diagnosa_13 == "K297 - Gastritis, unspecified" ~ NA,
      Diagnosa_13 == "K30 - Dyspepsia" ~ NA,
      Diagnosa_13 == "K529 - Noninfective gastroenteritis and colitis, unspecified" ~ NA,
      Diagnosa_13 == "L010 - Impeti [any organism] [any site]" ~ NA,
      Diagnosa_13 == "L020 - Cutaneous abscess, furuncle and carbuncle of face" ~ NA,
      Diagnosa_13 == "L022 - Cutaneous abscess, furuncle and carbuncle of trunk" ~ NA,
      Diagnosa_13 == "L028 - Cutaneous abscess, furuncle and carbuncle of other sites" ~ NA,
      Diagnosa_13 == "L080 - Pyoderma" ~ NA,
      Diagnosa_13 == "L081 - Erythrasma" ~ NA,
      Diagnosa_13 == "L089 - Local infection of skin and subcutaneous tissue, unspecified" ~ NA,
      Diagnosa_13 == "L200 - Besnier's pruri" ~ NA,
      Diagnosa_13 == "L210 - Seborrhoea capitis" ~ NA,
      Diagnosa_13 == "L240 - Irritant contact dermatitis due to detergents" ~ NA,
      Diagnosa_13 == "L270 - Generalized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_13 == "L271 - Localized skin eruption due to drugs and medicaments" ~ NA,
      Diagnosa_13 == "L272 - Dermatitis due to ingested food" ~ NA,
      Diagnosa_13 == "L300 - Nummular dermatitis" ~ NA,
      Diagnosa_13 == "L309 - Dermatitis, unspecified" ~ NA,
      Diagnosa_13 == "L42 - Pityriasis rosea" ~ NA,
      Diagnosa_13 == "L500 - Allergic urticaria" ~ NA,
      Diagnosa_13 == "L700 - Acne vulgaris" ~ NA,
      Diagnosa_13 == "L710 - Perioral dermatitis" ~ NA,
      Diagnosa_13 == "L732 - Hidradenitis suppurativa" ~ NA,
      Diagnosa_13 == "L743 - Miliaria, unspecified" ~ NA,
      Diagnosa_13 == "N12 - Tubulo-interstitial nephritis, not specified as acute or chronic" ~ NA,
      Diagnosa_13 == "N390 - Urinary tract infection, site not specified" ~ NA,
      Diagnosa_13 == "N47 - Redundant prepuce, phimosis and paraphimosis" ~ NA,
      Diagnosa_13 == "N508 - Other specified disorders of male genital organs" ~ NA,
      Diagnosa_13 == "N61 - Inflammatory disorders of breast" ~ NA,
      Diagnosa_13 == "N645 - Other signs and symptoms in breast" ~ NA,
      Diagnosa_13 == "N700 - Acute salpingitis and oophoritis" ~ NA,
      Diagnosa_13 == "N760 - Acute vaginitis" ~ NA,
      Diagnosa_13 == "N762 - Acute vulvitis" ~ NA,
      Diagnosa_13 == "N768 - Other specified inflammation of vagina and vulva" ~ NA,
      Diagnosa_13 == "O000 - Abdominal pregnancy" ~ NA,
      Diagnosa_13 == "O030 - Spontaneous abortion, incomplete, complicated by genital tract and pelvic infection" ~ NA,
      Diagnosa_13 == "O25 - Malnutrition in pregnancy" ~ NA,
      Diagnosa_13 == "O700 - First degree perineal laceration during delivery" ~ NA,
      Diagnosa_13 == "O701 - Second degree perineal laceration during delivery" ~ NA,
      Diagnosa_13 == "O702 - Third degree perineal laceration during delivery" ~ NA,
      Diagnosa_13 == "O703 - Fourth degree perineal laceration during delivery" ~ NA,
      Diagnosa_13 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_13 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_13 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_13 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_13 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_13 == "O921 - Cracked nipple associated with childbirth" ~ NA,
      Diagnosa_13 == "R040 - Epistaxis" ~ NA,
      Diagnosa_13 == "R560 - Febrile convulsions" ~ NA,
      Diagnosa_13 == "R51 - Headache" ~ NA,
      Diagnosa_13 == "T151 - Foreign body in conjunctival sac" ~ NA,
      Diagnosa_13 == "T171 - Foreign body in nostril" ~ NA,
      Diagnosa_13 == "T301 - Burn of first degree, body region unspecified" ~ NA,
      Diagnosa_13 == "T302 - Burn of second degree, body region unspecified" ~ NA,
      Diagnosa_13 == "T753 - Motion sickness" ~ NA,
      Diagnosa_13 == "T782 - Anaphylactic shock, unspecified" ~ NA,
      Diagnosa_13 == "T812 - Accidental puncture and laceration during a procedure, not elsewhere classified" ~ NA,
      Diagnosa_13 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_13 == "Z370 - Single live birth" ~ NA,
      TRUE ~ Diagnosa_13)) %>%
  select(-Diagnosa_13)

nonspes <- nonspes %>%
  mutate(Diagakhir = paste0(as.character(Diagnosa_1),";",
                            as.character(Diagnosa_2),";",
                            as.character(Diagnosa_3),";",
                            as.character(Diagnosa_4),";",
                            as.character(Diagnosa_5),";",
                            as.character(Diagnosa_6),";",
                            as.character(Diagnosa_7),";",
                            as.character(Diagnosa_8),";",
                            as.character(Diagnosa_9),";",
                            as.character(Diagnosa_010),";",
                            as.character(Diagnosa_011),";",
                            as.character(Diagnosa_012),";",
                            as.character(Diagnosa_013)))

nonspes$klasifikasi <- with(nonspes,
                              ifelse(Diagakhir == "NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA",
                                     "Non Spesialistik", "Spesialistik"))

nonspes <- nonspes %>% select(-Diagnosa,-Diagnosa_1,-Diagnosa_2,
                              -Diagnosa_3,-Diagnosa_4,-Diagnosa_5,
                              -Diagnosa_6,-Diagnosa_7,-Diagnosa_8,
                              -Diagnosa_9,-Diagnosa_010,-Diagnosa_011,
                              -Diagnosa_012,-Diagnosa_013,-Diagakhir)

spesialis <- data %>% subset(Tglpelayanan >= "2019-01-01") %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(!str_detect(Diagnosa,"A010|A059|A060|A150|A184|A270|A300|A35|A370|A46|A510|A520|A540|A90|A91|A920|B001|B009|B019|B029|B059|B07|B081|B150|B160|B170|B171|B172|B24|B260|B350|B351|B352|B353|B354|B356|B359|B360|B500|B510|B520|B530|B54|B650|B680|B740|B760|B769|B770|B780|B850|B853|B86|D179|D500|D649|E100|E110|E162|E46|E569|E610|E660|E740|E785|E790|F450|G430|G442|G470|G510|H000|H010|H020|H100|H113|H151|H162|H524|H531|H600|H612|H669|H810|I10|I839|I840|I880|J00|J020|J030|J040|J100|J120|J300|J304|J340|J40|J459|K210|K291|K297|K30|K529|L010|L020|L022|L028|L080|L081|L089|L200|L210|L240|L270|L271|L272|L300|L309|L42|L500|L700|L710|L732|L743|N12|N390|N47|N508|N61|N645|N700|N760|N762|N768|O000|O030|O25|O700|O701|O702|O703|O800|O801|O808|O809|O839|O921|R040|R560|R51|T151|T171|T301|T302|T753|T782|T812|Z719|Z370")) %>%
  select(-Diagnosa)
spesialis$klasifikasi <- "Spesialistik"

data240 <- rbind(nonspes,spesialis)

data24 <- data %>% subset(Tglpelayanan >= "2019-01-01")

df <- anti_join(data24,data240)
df$klasifikasi <- "Spesialistik"

data240 <- rbind(df,nonspes,spesialis)
rm(data,data24,df,nonspes,spesialis)

data240 <- data240 %>%
  mutate(
    Kddiagprimer1 = case_when(
      Kddiagprimer == "Z039" ~ NA,
      Kddiagprimer == "Z041" ~ NA,
      Kddiagprimer == "Z359" ~ NA,
      Kddiagprimer == "Z491" ~ NA,
      Kddiagprimer == "Z501" ~ NA,
      Kddiagprimer == "Z719" ~ NA,
      Kddiagprimer == "Z088" ~ NA,
      Kddiagprimer == "Z089" ~ NA,
      Kddiagprimer == "Z090" ~ NA,
      Kddiagprimer == "Z091" ~ NA,
      Kddiagprimer == "Z092" ~ NA,
      Kddiagprimer == "Z093" ~ NA,
      Kddiagprimer == "Z094" ~ NA,
      Kddiagprimer == "Z095" ~ NA,
      Kddiagprimer == "Z096" ~ NA,
      Kddiagprimer == "Z097" ~ NA,
      Kddiagprimer == "Z098" ~ NA,
      Kddiagprimer == "Z099" ~ NA,
      Kddiagprimer == "Z340" ~ NA,
      Kddiagprimer == "Z349" ~ NA,
      Kddiagprimer == "Z504" ~ NA,
      Kddiagprimer == "Z509" ~ NA,
      Kddiagprimer == "Z549" ~ NA,
      Kddiagprimer == "Z898" ~ NA,
      Kddiagprimer == "Z908" ~ NA,
      Kddiagprimer == "Z961" ~ NA,
      Kddiagprimer == "Z988" ~ NA,
      Kddiagprimer == "Z489" ~ NA,
      TRUE ~ Kddiagprimer))
data240$Nmdiagprimer1 <- with(data240,
                              ifelse(is.na(data240$Kddiagprimer1),NA,Nmdiagprimer))
data240 <- data240 %>%
  mutate(Diagprimer = paste0(as.character(Kddiagprimer1)," - ",
                             as.character(Nmdiagprimer1)))
data240$Diagprimer <- as.character(trimws(gsub("NA - NA",NA,data240$Diagprimer)), "both")
data240 <- data240 %>%
  mutate(Diagnosa = paste0(as.character(Diagprimer),";",
                           as.character(Diagsekunder)))
data240$Diagnosa <- as.character(trimws(gsub(";NA|NA;|NA","",data240$Diagnosa)), "both")

data240 <- data240 %>% select(-Kddiagprimer1,-Nmdiagprimer1,-Diagprimer)

data240 <- data240[order(data240$Nokapst,data240$Tglplgsjp),]
data240 <- data240 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
data240$jeda <- as.integer(data240$Tgldtgsjp) - as.integer(data240$tgl_before)
data240$jeda <- as.numeric(data240$jeda) %>%
  replace(is.na(.), "")
data240$jeda <- as.factor(data240$jeda)

data240$Diagnosa <- with(data240,
                         ifelse(Diagnosa == "E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension",
                                      "I10 - Essential (primary) hypertension;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                ifelse(Diagnosa == "E785 - Hyperlipidaemia, unspecified;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                       "E119 - Non-insulin-dependent diabetes mellitus without complications;E785 - Hyperlipidaemia, unspecified",
                                       ifelse(Diagnosa == "I10 - Essential (primary) hypertension;K30 - Dyspepsia",
                                              "K30 - Dyspepsia;I10 - Essential (primary) hypertension",
                                              ifelse(Diagnosa == "E119 - Non-insulin-dependent diabetes mellitus without complications;K30 - Dyspepsia",
                                                     "K30 - Dyspepsia;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                     ifelse(Diagnosa == "E785 - Hyperlipidaemia, unspecified;I10 - Essential (primary) hypertension;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                            "I10 - Essential (primary) hypertension;E119 - Non-insulin-dependent diabetes mellitus without complications;E785 - Hyperlipidaemia, unspecified",
                                                            ifelse(Diagnosa == "E785 - Hyperlipidaemia, unspecified;E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension",
                                                                   "I10 - Essential (primary) hypertension;E119 - Non-insulin-dependent diabetes mellitus without complications;E785 - Hyperlipidaemia, unspecified",
                                                                   ifelse(Diagnosa == "I10 - Essential (primary) hypertension;E785 - Hyperlipidaemia, unspecified;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                                          "I10 - Essential (primary) hypertension;E119 - Non-insulin-dependent diabetes mellitus without complications;E785 - Hyperlipidaemia, unspecified",
                                                                          Diagnosa))))))))

#========================================================================
write.csv(data240, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_data24.csv",
          na="", row.names = FALSE)
#========================================================================
data1 <- data %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppklayan,
          Nosjp,Norjkawalsep,Sumber,Nmtkp,Poli_asal,Politujsjp,
          Tglpelayanan,tgl_before,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Sumber == "Rujukan Internal") %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"A010|A059|A060|A150|A184|A270|A300|A35|A370|A46|A510|A520|A540|A90|A91|A920|B001|B009|B019|B029|B059|B07|B081|B150|B160|B170|B171|B172|B24|B260|B350|B351|B352|B353|B354|B356|B359|B360|B500|B510|B520|B530|B54|B650|B680|B740|B760|B769|B770|B780|B850|B853|B86|D179|D500|D649|E100|E110|E162|E46|E569|E610|E660|E740|E785|E790|F450|G430|G442|G470|G510|H000|H010|H020|H100|H113|H151|H162|H520|H521|H522|H524|H531|H600|H612|H669|H810|I10|I839|I840|I880|J00|J020|J030|J040|J100|J120|J300|J304|J340|J40|J459|K210|K291|K529|L010|L020|L022|L028|L080|L081|L089|L200|L210|L240|L270|L271|L272|L300|L309|L42|L500|L700|L710|L732L743|N12|N390|N47|N508|N61|N645|N700|N760|N762|N768|O000|O030|O25|O700|O701|O702|O703|O800|O801|O808|O809|O839|O921|R040|R560|T151|T171|T301|T302|T753|T782|T812")) %>%
  filter(!str_detect(Diagnosa,"H52")) %>%
  select(-Diagnosa)

data1 <- data1[order(data1$Nokapst,data1$Tglplgsjp),]
data1 <- data1 %>% subset(Tglpelayanan >= "2024-01-01")

#========================================================================
write.xlsx(data1, file = "rujukan int nonspesialis.xlsx")
#========================================================================

df_data$Namadpjp01 <- trimws(gsub("dr.", "", tolower(df_data$Namadpjp)),"both")
df_data$Namadpjp01 <- trimws(gsub("dr", "", df_data$Namadpjp01),"both")
df_data$Namadpjp01 <- str_replace_all(df_data$Namadpjp01, "[^[:alnum:]]", " ")
df_data$Namadpjp01 <- trimws(gsub("sp", "sp ", df_data$Namadpjp01),"both")
df_data$Namadpjp01 <- gsub("\\s+"," ",df_data$Namadpjp01)
df_data$Namadpjp <- NULL
df_data <- df_data %>% rename(Namadpjp = Namadpjp01)
#========================================================================
#CMG, Specific CBG, SEVERITY
df_data$CMG <- as.character(trimws(substr(df_data$Kdinacbgs,1,1)), "both")
df_data$CBG <- as.character(trimws(substr(df_data$Kdinacbgs,1,6)), "both")
df_data$Spec <- as.character(trimws(substr(df_data$Kdinacbgs,5,6)), "both")
df_data$Kdsevel <- as.character(trimws(substr(df_data$Kdinacbgs,8,10)), "both")
df_data <- df_data %>%
  mutate(
    Sevel = case_when(
      Kdsevel == "0" ~ "0",
      Kdsevel == "I" ~ "1",
      Kdsevel == "II" ~ "2",
      Kdsevel == "III" ~ "3",
      TRUE ~ Kdsevel)) %>%
  select(-Kdsevel) 

df_data$tp <- as.character(trimws(substr(df_data$Kdinacbgs,3,3)), "both")
df_data <- data.frame(df_data)
df_data <- df_data %>%
  mutate(
    tipe = case_when(
      tp == "1" ~ "Prosedur Rawat Inap",
      tp == "2" ~ "Prosedur Besar Rawat Jalan",
      tp == "3" ~ "Prosedur Signifikan Rawat Jalan",
      tp == "4" ~ "Rawat Inap Bukan Prosedur",
      tp == "5" ~ "Rawat Jalan Bukan Prosedur",
      tp == "6" ~ "Rawat Inap Kebidanan",
      tp == "7" ~ "Rawat Jalan Kebidanan",
      tp == "8" ~ "Rawat Inap Neonatal",
      tp == "9" ~ "Rawat Jalan Neonatal",
      tp == "0" ~ "Error",
      TRUE ~ tp)) %>%
  select(-tp)
df_data$Nminacbgs <- as.character(trimws(df_data$Nminacbgs), "both")
df_data$kel_umur <- with(df_data,
                      ifelse(Umur >= 0 & Umur < 1, "< 1 Tahun",
                             ifelse(Umur >= 1 & Umur < 5, "1-4 Tahun",
                                    ifelse(Umur >= 5 & Umur < 12, "5-11 Tahun",
                                           ifelse(Umur >= 12 & Umur < 17, "12-16 Tahun",
                                                  ifelse(Umur >= 17 & Umur <= 25, "17-25 Tahun",
                                                         ifelse(Umur > 25 & Umur <= 35, "26-35 Tahun",
                                                                ifelse(Umur > 35 & Umur <= 45, "36-45 Tahun",
                                                                       ifelse(Umur > 45 & Umur <= 55, "46-55 Tahun",
                                                                              ifelse(Umur > 55 & Umur <= 65, "56-65 Tahun",
                                                                                     ifelse(Umur > 65 & Umur < 75, "66-74 Tahun",
                                                                                            ifelse(Umur >= 75 & Umur <= 90, "75-90 Tahun",
                                                                                                   ifelse(Umur > 90, "> 90 Tahun",
                                                                                                          "Salah")))))))))))))


#==============================================================================
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")

df18 <- read_csv("Sheet_1_Full_Data_data (2018).csv") %>%
  select(Nosep,Norjkawalsep,Sumber,jenisrujukaninternal) %>%
  rename(Nosjp = Nosep,
         Sumber2 = Sumber)

df19 <- read_csv("Sheet_1_Full_Data_data (2019).csv") %>%
  select(Nosep,Norjkawalsep,Sumber,jenisrujukaninternal) %>%
  rename(Nosjp = Nosep,
         Sumber2 = Sumber)

df20 <- read_csv("Sheet_1_Full_Data_data (2020).csv") %>%
  select(Nosep,Norjkawalsep,Sumber,jenisrujukaninternal) %>%
  rename(Nosjp = Nosep,
         Sumber2 = Sumber)

df21 <- read_csv("Sheet_1_Full_Data_data (2021).csv") %>%
  select(Nosep,Norjkawalsep,Sumber,jenisrujukaninternal) %>%
  rename(Nosjp = Nosep,
         Sumber2 = Sumber)

df22 <- read_csv("Sheet_1_Full_Data_data (2022).csv") %>%
  select(Nosep,Norjkawalsep,Sumber,jenisrujukaninternal) %>%
  rename(Nosjp = Nosep,
         Sumber2 = Sumber)

df23 <- read_csv("Sheet_1_Full_Data_data (2023).csv") %>%
  select(Nosep,Norjkawalsep,Sumber,jenisrujukaninternal) %>%
  rename(Nosjp = Nosep,
         Sumber2 = Sumber)

df24 <- read_csv("Sheet_1_Full_Data_data (2024).csv") %>%
  select(Nosep,Norjkawalsep,Sumber,jenisrujukaninternal) %>%
  rename(Nosjp = Nosep,
         Sumber2 = Sumber)

df <- rbind(df18,df19,df20,df21,df22,df23,df24)
rm(df18,df19,df20,df21,df22,df23,df24)

df$Sumber <- with(df,
                  ifelse(is.na(df$jenisrujukaninternal),
                         Sumber2,jenisrujukaninternal))
cek <- df %>% subset(is.na(Sumber))
rm(cek)
df <- df %>% select(-Sumber2,-jenisrujukaninternal)

cek <- df %>% subset(is.na(Sumber))
rm(cek)
#=========================================================================
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")
load("df_rujukan.rda")

df_ruj <- df_ruj %>% select(nokapst,no_kunjungan,Diagnosa)
df_ruj <- df_ruj %>% rename(Diagmasuk = Diagnosa)
#=========================================================================
df1 <- left_join(df,df_ruj, by = c("Norjkawalsep"="no_kunjungan"))
cek <- df1 %>%
  subset(Sumber == "Rujukan FKTP") %>%
  subset(is.na(Diagmasuk))
rm(cek,df,df_ruj)

data <- left_join(df_data,df1, by = c("Nosjp"="Nosjp"))

cek <- data %>% subset(Nokapst != nokapst)
rm(cek)
data <- data %>% select(-nokapst)
rm(df1,df_data)

data <- data[order(data$Nokapst,data$Tglplgsjp),]
data <- data %>%
  group_by(Nokapst) %>%
  mutate(Poli_asal = lag(Politujsjp,n=1))

datarj <- data %>% subset(Nmtkp == "RJTL")
datari <- data %>% subset(Nmtkp == "RITL")

datarj <- datarj[order(datarj$Nokapst,datarj$Tglplgsjp),]

datarjIGD <- datarj %>%
  subset(is.na(Norjkawalsep)) %>%
  subset(Politujsjp == "Instalasi Gawat Darurat")
cek <- datarjIGD %>% subset(is.na(Sumber))
cek$Sumber <- "UGD"
dataeror <- datarjIGD %>% subset(!is.na(Sumber))
datarjIGD <- rbind(cek,dataeror)
rm(cek,dataeror)

datarjnonIGD <- datarj %>%
  subset(Politujsjp != "Instalasi Gawat Darurat")
datarjnonIGD <- datarjnonIGD[order(datarjnonIGD$Nokapst,datarjnonIGD$Tglplgsjp),]

datarjnonIGD <- datarjnonIGD %>%
  group_by(Nokapst,Norjkawalsep) %>%
  mutate(Poli_asal = first(Politujsjp))

dataempty <- datarjnonIGD %>% subset(is.na(Sumber))
dataempty$Sumber2 <- with(dataempty,
                          ifelse(Politujsjp == Poli_asal,
                                 "Kontrol Ulang","Rujukan Internal"))
dataempty <- dataempty %>% select(-Sumber)
dataempty <- dataempty %>% rename(Sumber = Sumber2)

datarjfktp <- datarjnonIGD %>% subset(Sumber == "Rujukan FKTP")
datarjantars <- datarjnonIGD %>% subset(Sumber == "Rujukan Antar RS")
datarjkontri <- datarjnonIGD %>% subset(Sumber %in% c("Rujukan Internal",
                                                      "Kontrol Ulang"))

cek <- datarjkontri %>% subset(is.na(Sumber))
rm(cek)

datarjkontri$Sumber2 <- with(datarjkontri,
                             ifelse(Politujsjp == Poli_asal,
                                    "Kontrol Ulang","Rujukan Internal"))
datarjkontri <- datarjkontri %>% select(-Sumber)
datarjkontri <- datarjkontri %>% rename(Sumber = Sumber2)

datarj <- rbind(datarjIGD,datarjfktp,datarjkontri,datarjantars,dataempty)
rm(datarjIGD,datarjnonIGD,datarjfktp,datarjkontri,datarjantars,dataempty)

data <- rbind(datari,datarj)
rm(datari,datarj)
data <- data[order(data$Nokapst,data$Tglplgsjp),]
#=======================================================================
cek$Sumber <- NULL 
cek <- cek %>% rename(Sumber = Sumber3)
dataeror <- datarjnonIGD %>%
  subset(!is.na(Sumber))

datarjnonIGD <- rbind(dataeror,cek)
rm(dataeror,cek)

datarjnonIGD <- datarjnonIGD[order(datarjnonIGD$Nokapst,datarjnonIGD$Tglplgsjp),]
#=======================================================================
datarj <- rbind(datarjnonIGD,datarjIGD)
rm(datarjnonIGD,datarjIGD)
datarj <- datarj[order(datarj$Nokapst,datarj$Tglplgsjp),]
cek <- datarj %>% subset(is.na(Sumber))
rm(cek)

data <- rbind(datarj,datari)
rm(datarj,datari)


data <- data %>% select(-Sumber2,-jenisrujukaninternal)

data <- data[order(data$Nokapst,data$Tglplgsjp),]
data <- data %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
data$jeda <- as.integer(data$Tgldtgsjp) - as.integer(data$tgl_before)
data$jeda <- as.numeric(data$jeda) %>%
  replace(is.na(.), "")
data$jeda <- as.factor(data$jeda)

data$Umur <- abs(data$Umur)
data$kel_umur <- with(data,
                      ifelse(Umur >= 0 & Umur < 1, "< 1 Tahun",
                             ifelse(Umur >= 1 & Umur < 5, "1-4 Tahun",
                                    ifelse(Umur >= 5 & Umur < 12, "5-11 Tahun",
                                           ifelse(Umur >= 12 & Umur < 17, "12-16 Tahun",
                                                  ifelse(Umur >= 17 & Umur <= 25, "17-25 Tahun",
                                                         ifelse(Umur > 25 & Umur <= 35, "26-35 Tahun",
                                                                ifelse(Umur > 35 & Umur <= 45, "36-45 Tahun",
                                                                       ifelse(Umur > 45 & Umur <= 55, "46-55 Tahun",
                                                                              ifelse(Umur > 55 & Umur <= 65, "56-65 Tahun",
                                                                                     ifelse(Umur > 65 & Umur < 75, "66-74 Tahun",
                                                                                            ifelse(Umur >= 75 & Umur <= 90, "75-90 Tahun",
                                                                                                   ifelse(Umur > 90, "> 90 Tahun",
                                                                                                          "Salah")))))))))))))

save(data, file = "ur_all.rda")
load("ur_all.rda")

cek <- data %>%
  subset(kel_umur == "Salah")

write.csv(data, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_all.csv",
          na="", row.names = FALSE)
#==============================================================================
data <- data %>%
  select(-Umur.y) %>%
  rename(Umur = Umur.x)
#8. kasus dirujuk dengan over biaya RS
data <- data %>%
  filter(str_detect(Nmtkp,c("RITL"))) %>%
  filter(str_detect(Nmjnspulang,c("Rujuk")))%>%
  group_by(Nokapst) %>%
  mutate(netbiaya = Biayaverifikasi-biayars)
data$netbiaya <- as.integer(ifelse(data$netbiaya <= 0, 1, 0))

#7. kasus fako pbpu mandiri sebelum satu bulan
%>%
  filter(str_detect(Nmjnspulang,c("Rujuk"))) %>%
  filter(str_detect(Procedure,c("1341|1371|992")))

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")
data$LOS <- as.integer(data$Tglplgsjp) - as.integer(data$Tgldtgsjp) +1

#Transform Nmtkp menjadi 1&2 --> 1= berarti RITL, 2= berarti RJTL
data$Nmtkp[data$Nmtkp=="RITL"] <- "1"
data$Nmtkp[data$Nmtkp=="RJTL"] <- "2"
data$Nmtkp <- as.factor(data$Nmtkp)

#Transform Jkpst menjadi 1&2 --> 1= berarti Laki-laki, 2= berarti Perempuan
data$Jkpst[data$Jkpst=="Laki-laki"] <- "1"
data$Jkpst[data$Jkpst=="Perempuan"] <- "2"
data$Jkpst <- as.factor(data$Jkpst)

#Transform Kelashak menjadi 1,2,3
data$Kelashak[data$Kelashak=="Kelas I"] <- "1"
data$Kelashak[data$Kelashak=="Kelas II"] <- "2"
data$Kelashak[data$Kelashak=="Kelas III"] <- "3"
data$Kelashak <- as.factor(data$Kelashak)

#Transform Klsrawat menjadi 1,2,3
data$Klsrawat[data$Klsrawat=="Kelas I"] <- "1"
data$Klsrawat[data$Klsrawat=="Kelas II"] <- "2"
data$Klsrawat[data$Klsrawat=="Kelas III"] <- "3"
data$Klsrawat <- as.factor(data$Klsrawat)
#========================================================================
data <- data %>% rename(DM = Kddiagmasuk,
                        NmDM = Nmdiagmasuk,
                        Poli = Politujsjp,
                        Sumber = Sumberkunjungan,
                        SEP = Nosjp,
                        DU =  Kddiagprimer, 
                        NmDU = Nmdiagprimer,
                        DS = Diagsekunder,
                        Start = Tgldtgsjp,
                        End = Tglplgsjp,
                        Pulang = Nmjnspulang,
                        BiayaRS = biayars,
                        Biaya = Biayaverifikasi)

colnames(data)
glimpse(data)
str(data)

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")
load("datasep.rda")
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")

dfcm <- left_join(data, df, by = c("SEP"="Nosep")) %>%
  select(-Sumber.y) %>%
  rename(Sumber = Sumber.x)
rm(data,df)

data <- data[order(data$Nokapst, data$End),]
dfcm <- dfcm %>%
  group_by(Nokapst) %>%
  mutate(Prvtkp = lag(Nmtkp))

dfcm$Prvtkp <- as.numeric(dfcm$Prvtkp) %>%
  replace(is.na(.), "")
dfcm$Prvtkp <- as.factor(dfcm$Prvtkp)

dfcm <- dfcm[order(dfcm$Nokapst, dfcm$End),]
dfcm <- dfcm %>%
  group_by(Nokapst) %>%
  mutate(interval = End - lag(End))

dfcm$interval <- as.numeric(dfcm$interval) %>%
  replace(is.na(.), "")
dfcm$interval <- as.factor(dfcm$interval)

dfcm <- dfcm %>% subset(Tglpelayanan >= "2021-12-01")

dfcm$Namadpjp01 <- trimws(gsub("dr.", "", tolower(dfcm$Namadpjp)),"both")
dfcm$Namadpjp01 <- trimws(gsub("dr", "", dfcm$Namadpjp01),"both")
dfcm$Namadpjp01 <- str_replace_all(dfcm$Namadpjp01, "[^[:alnum:]]", " ")
dfcm$Namadpjp01 <- trimws(gsub("sp", "sp ", dfcm$Namadpjp01),"both")
dfcm$Namadpjp01 <- gsub("\\s+"," ",dfcm$Namadpjp01)
dfcm <- dfcm %>% rename(DPJP = Namadpjp01)
dfcm$Namadpjp <- NULL
#========================================================================

#CMG, Specific CBG, SEVERITY
dfcm$CMG <- as.character(trimws(substr(dfcm$Kdinacbgs,1,1)), "both")
dfcm$CBG <- as.character(trimws(substr(dfcm$Kdinacbgs,1,6)), "both")
dfcm$Spec <- as.character(trimws(substr(dfcm$Kdinacbgs,5,6)), "both")
dfcm$Kdsevel <- as.character(trimws(substr(dfcm$Kdinacbgs,8,10)), "both")
dfcm <- dfcm %>% 
  mutate(
    Sevel = case_when(
      Kdsevel == "0" ~ "0",
      Kdsevel == "I" ~ "1",
      Kdsevel == "II" ~ "2",
      Kdsevel == "III" ~ "3",
      TRUE ~ Kdsevel)) %>%
  select(-Kdsevel) 

dfcm$tp <- as.character(trimws(substr(dfcm$Kdinacbgs,3,3)), "both")
dfcm <- data.frame(dfcm)
dfcm <- dfcm %>% 
  mutate(
    tipe = case_when(
      tp == "1" ~ "Prosedur Rawat Inap",
      tp == "2" ~ "Prosedur Besar Rawat Jalan",
      tp == "3" ~ "Prosedur Signifikan Rawat Jalan",
      tp == "4" ~ "Rawat Inap Bukan Prosedur",
      tp == "5" ~ "Rawat Jalan Bukan Prosedur",
      tp == "6" ~ "Rawat Inap Kebidanan",
      tp == "7" ~ "Rawat Jalan Kebidanan",
      tp == "8" ~ "Rawat Inap Neonatal",
      tp == "9" ~ "Rawat Jalan Neonatal",
      tp == "0" ~ "Error",
      TRUE ~ tp)) %>%
  select(-tp)

library(splitstackshape)
dfcm <- concat.split(dfcm, "DS", ";")
colnames(dfcm)

dfcm$DS01 <- as.character(trimws(gsub("-","",substr(dfcm$DS_1,1,6)), "both"))
dfcm$DS01 [is.na(dfcm$DS01)] <- ""
dfcm$DS01 <- as.factor(dfcm$DS01)
dfcm$DS02 <- as.character(trimws(gsub("-","",substr(dfcm$DS_2,1,6)), "both"))
dfcm$DS02 [is.na(dfcm$DS02)] <- ""
dfcm$DS02 <- as.factor(dfcm$DS02)
dfcm$DS03 <- as.character(trimws(gsub("-","",substr(dfcm$DS_3,1,6)), "both"))
dfcm$DS03 [is.na(dfcm$DS03)] <- ""
dfcm$DS03 <- as.factor(dfcm$DS03)
dfcm$DS04 <- as.character(trimws(gsub("-","",substr(dfcm$DS_4,1,6)), "both"))
dfcm$DS04 [is.na(dfcm$DS04)] <- ""
dfcm$DS04 <- as.factor(dfcm$DS04)
dfcm$DS05 <- as.character(trimws(gsub("-","",substr(dfcm$DS_5,1,6)), "both"))
dfcm$DS05 [is.na(dfcm$DS05)] <- ""
dfcm$DS05 <- as.factor(dfcm$DS05)
dfcm$DS06 <- as.character(trimws(gsub("-","",substr(dfcm$DS_6,1,6)), "both"))
dfcm$DS06 [is.na(dfcm$DS06)] <- ""
dfcm$DS06 <- as.factor(dfcm$DS06)
dfcm$DS07 <- as.character(trimws(gsub("-","",substr(dfcm$DS_7,1,6)), "both"))
dfcm$DS07 [is.na(dfcm$DS07)] <- ""
dfcm$DS07 <- as.factor(dfcm$DS07)
#======================================================================
dfcm <- concat.split(dfcm, "Procedure", ";")

colnames(dfcm)

dfcm$Proc01 <- as.character(trimws(gsub("-","",substr(dfcm$Procedure_1,1,4)), "both"))
dfcm$Proc01 [is.na(dfcm$Proc01)] <- ""
dfcm$Proc01 <- as.factor(dfcm$Proc01)
dfcm$Proc02 <- as.character(trimws(gsub("-","",substr(dfcm$Procedure_2,1,4)), "both"))
dfcm$Proc02 [is.na(dfcm$Proc02)] <- ""
dfcm$Proc02 <- as.factor(dfcm$Proc02)
dfcm$Proc03 <- as.character(trimws(gsub("-","",substr(dfcm$Procedure_3,1,4)), "both"))
dfcm$Proc03 [is.na(dfcm$Proc03)] <- ""
dfcm$Proc03 <- as.factor(dfcm$Proc03)
dfcm$Proc04 <- as.character(trimws(gsub("-","",substr(dfcm$Procedure_4,1,4)), "both"))
dfcm$Proc04 [is.na(dfcm$Proc04)] <- ""
dfcm$Proc04 <- as.factor(dfcm$Proc04)

##DATA_03 = Gabung DIAG & PROC
#remove empty cell with NA
#============================================================================
library(readxl)
setwd("D:/data gresik/UR/Data Sampel BPJS Kesehatan 2015-2020 v01/Metadata")
DIAGFKTP <- read_excel("Kode ICD10 untuk diagnosis FKTP Kapitasi.xlsx", 
                       sheet = "Sheet1", range = NULL, 
                       col_names = TRUE, col_types = NULL, na = "",
                       trim_ws = TRUE, skip = 0)
colnames(dfcm)
dfcm00 <- dfcm %>%
  mutate(DP = as.factor(substr(dfcm$DU,1,3)),
         DS_01 = as.factor(substr(dfcm$DS01,1,3)),
         DS_02 = as.factor(substr(dfcm$DS02,1,3)),
         DS_03 = as.factor(substr(dfcm$DS03,1,3)),
         DS_04 = as.factor(substr(dfcm$DS04,1,3)),
         DS_05 = as.factor(substr(dfcm$DS05,1,3)),
         DS_06 = as.factor(substr(dfcm$DS06,1,3)),
         DS_07 = as.factor(substr(dfcm$DS07,1,3)))
rm(dfcm)

dfcm00$DIAG3 <- paste(dfcm00$DP,dfcm00$DS_01,dfcm00$DS_02,dfcm00$DS_03,dfcm00$DS_04,
                      dfcm00$DS_05,dfcm00$DS0_6,sep=" ")
dfcm00$DIAG3 <- trimws(dfcm00$DIAG3, whitespace = " ")
dfcm00$DIAG3 <- gsub(" ",", ",dfcm00$DIAG3)
dfcm00$DIAG3 <- sapply(dfcm00$DIAG3,
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))
PROC <- read.csv2("sub chapter proc.csv",
                  colClasses=c("factor", "character", "character"))
glimpse(PROC)

dfcm01 <- dfcm00 %>%
  mutate(Proc_01 = as.factor(substr(dfcm00$Proc01,1,2)),
         Proc_02 = as.factor(substr(dfcm00$Proc02,1,2)),
         Proc_03 = as.factor(substr(dfcm00$Proc03,1,2)),
         Proc_04 = as.factor(substr(dfcm00$Proc04,1,2)))
rm(dfcm00)

dfcm01$PROC2 <- paste(dfcm01$Proc_01,dfcm01$Proc_02,dfcm01$Proc_03,
                      dfcm01$Proc_04,sep=" ")
dfcm01$PROC2 <- trimws(dfcm01$PROC2, whitespace = " ")
dfcm01$PROC2 <- gsub(" ",", ",dfcm01$PROC2)
dfcm01$PROC2 <- sapply(dfcm01$PROC2,
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))

PROC2 <- read.csv2("Chapter Procedure.csv", 
                   colClasses=c("factor", "character",
                                "character", "character"))

dfcm02 <- dfcm01 %>%
  mutate(Proc_001 = as.factor(substr(dfcm01$Proc01,1,3)),
         Proc_002 = as.factor(substr(dfcm01$Proc02,1,3)),
         Proc_003 = as.factor(substr(dfcm01$Proc03,1,3)),
         Proc_004 = as.factor(substr(dfcm01$Proc04,1,3)))
rm(dfcm01)

dfcm02$PROC3 <- paste(dfcm02$Proc_001,dfcm02$Proc_002,dfcm02$Proc_003,dfcm02$Proc_004,
                      dfcm02$Proc_005,dfcm02$Proc_006,dfcm02$Proc_007,sep=" ")
dfcm02$PROC3 <- trimws(dfcm02$PROC3, whitespace = " ")
dfcm02$PROC3 <- gsub(" ",", ",dfcm02$PROC3)
dfcm02$PROC3 <- sapply(dfcm02$PROC3,
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))

dfcm02 <- dfcm02 %>%
  select(-DP,-DS_01,-DS_02,-DS_03,-DS_04,-DS_05,-DS_06,-DS_07,-Proc_01,
         -Proc_02,-Proc_03,-Proc_04,-Proc_001,-Proc_002,-Proc_003,-Proc_004)
dfcm02 <- concat.split(dfcm02, "DIAG3", ", ")
dfcm02 <- concat.split(dfcm02, "PROC2", ", ")
dfcm02 <- concat.split(dfcm02, "PROC3", ", ")
colnames(dfcm02)
#===========================================================================
#Kode ICD10 untuk diagnosis FKTP Kapitasi.xlsx

dfcm03 <- left_join(dfcm02, DIAGFKTP, by = c("DIAG3_1"="ICD10_Code")) %>%
  select(-DIAG3_1, -FKP14) %>%
  rename(DIAG3_1 = ICD10_Text)
dfcm03$DIAG3_1 [is.na(dfcm03$DIAG3_1)] <- ""
rm(dfcm02)
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_2"="ICD10_Code")) %>%
  select(-DIAG3_2, -FKP14) %>%
  rename(DIAG3_2 = ICD10_Text)
dfcm03$DIAG3_2 [is.na(dfcm03$DIAG3_2)] <- ""
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_3"="ICD10_Code")) %>%
  select(-DIAG3_3, -FKP14) %>%
  rename(DIAG3_3 = ICD10_Text)
dfcm03$DIAG3_3 [is.na(dfcm03$DIAG3_3)] <- ""
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_4"="ICD10_Code")) %>%
  select(-DIAG3_4, -FKP14) %>%
  rename(DIAG3_4 = ICD10_Text)
dfcm03$DIAG3_4 [is.na(dfcm03$DIAG3_4)] <- ""
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_5"="ICD10_Code")) %>%
  select(-DIAG3_5, -FKP14) %>%
  rename(DIAG3_5 = ICD10_Text)
dfcm03$DIAG3_5 [is.na(dfcm03$DIAG3_5)] <- ""
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_6"="ICD10_Code")) %>%
  select(-DIAG3_6, -FKP14) %>%
  rename(DIAG3_6 = ICD10_Text)
dfcm03$DIAG3_6 [is.na(dfcm03$DIAG3_6)] <- ""
#---------------------------------------------------------------------------
#Kode ICD9CM untuk Procedure Sub Chapter

dfcm03$PROC2_1 <- factor(dfcm03$PROC2_1)
dfcm03$PROC2_2 <- factor(dfcm03$PROC2_2)
dfcm03$PROC2_3 <- factor(dfcm03$PROC2_3)

dfcm04 <- left_join(dfcm03, PROC,
                  by = c("PROC2_1"="No.Sub.Chap")) %>%
  select(-PROC2_1, -Chap.Proc) %>%
  rename(PROC2_1 = Sub.Chap.Proc)
dfcm03$Proc_01 [is.na(dfcm03$Proc_01)] <- ""
rm(dfcm03)

dfcm04 <- left_join(dfcm04, PROC,
                    by = c("PROC2_2"="No.Sub.Chap")) %>%
  select(-PROC2_2, -Chap.Proc) %>%
  rename(PROC2_2 = Sub.Chap.Proc)
dfcm04$PROC2_2 [is.na(dfcm04$PROC2_2)] <- ""
dfcm04 <- left_join(dfcm04, PROC,
                    by = c("PROC2_3"="No.Sub.Chap")) %>%
  select(-PROC2_3, -Chap.Proc) %>%
  rename(PROC2_3 = Sub.Chap.Proc)
dfcm04$PROC2_3 [is.na(dfcm04$PROC2_3)] <- ""
#---------------------------------------------------------------------------
#Kode ICD9CM untuk Procedure Chapter

dfcm04$PROC3_1 <- factor(dfcm04$PROC3_1)
dfcm04$PROC3_2 <- factor(dfcm04$PROC3_2)
dfcm04$PROC3_3 <- factor(dfcm04$PROC3_3)
dfcm04$PROC3_4 <- factor(dfcm04$PROC3_4)

dfcm05 <- left_join(dfcm04, PROC2, by = c("PROC3_1"="No.Sub.Chap")) %>%
  select(-PROC3_1, -Prosedur, -Chap.Proc) %>%
  rename(PROC3_1 = Sub.Chap.Proc)
dfcm05$PROC3_1 [is.na(dfcm05$PROC3_1)] <- ""
rm(dfcm04)

dfcm05 <- left_join(dfcm05, PROC2, by = c("PROC3_2"="No.Sub.Chap")) %>%
  select(-PROC3_2, -Prosedur, -Chap.Proc) %>%
  rename(PROC3_2 = Sub.Chap.Proc)
dfcm05$PROC3_2 [is.na(dfcm05$PROC3_2)] <- ""
dfcm05 <- left_join(dfcm05, PROC2, by = c("PROC3_3"="No.Sub.Chap")) %>%
  select(-PROC3_3, -Prosedur, -Chap.Proc) %>%
  rename(PROC3_3 = Sub.Chap.Proc)
dfcm05$PROC3_3 [is.na(dfcm05$PROC3_3)] <- ""
dfcm05 <- left_join(dfcm05, PROC2, by = c("PROC3_4"="No.Sub.Chap")) %>%
  select(-PROC3_4, -Prosedur, -Chap.Proc) %>%
  rename(PROC3_4 = Sub.Chap.Proc)
dfcm05$PROC3_4 [is.na(dfcm05$PROC3_4)] <- ""

dfcm05 <- dfcm05 %>%
  mutate(DIAGP = as.factor(substr(dfcm05$DU,1,3)))

rm(DIAGFKTP,PROC,PROC2)

dfcm05 <- dfcm05 %>%
  select(-DS01,-DS02,-DS03,-DS04,-DS05,-DS06,-DS07,-Proc01,-Proc02,
         -Proc03,-Proc04)
dfcm05 <- dfcm05[order(dfcm05$Nokapst, dfcm05$End),]

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")
save(dfcm05, file="dfcm05.rda")

write.csv(dfcm05, "D:/data gresik/MTF KC GRESIK/dfcm.csv",
          na="", row.names = FALSE)


data$DIAGSEK <- paste(data$DS01,data$DS02,data$DS03,data$DS04,
                      data$DS05,data$DS06,data$DS07,data$DS08,
                      data$DS09,data$DS10,data$DS11,data$DS12,
                      sep=", ")
data$DIAGSEK <- gsub("NA, ","",data$DIAGSEK)
data$DIAGSEK <- gsub(", NA","",data$DIAGSEK)
data$DIAGSEK <- gsub("NA","",data$DIAGSEK)
data$DIAGSEK <- sapply(data$DIAGSEK, 
                    function(x) paste(unique(unlist(str_split(x,", "))), 
                                      collapse = ", "))
#=================================================================
#--------------------------LEFT JOIN------------------------------
#=================================================================

colnames(data)
data00 <- data %>%
  mutate(DP = as.factor(substr(data$DU,1,3)),
         DS_01 = as.factor(substr(data$DS01,1,3)),
         DS_02 = as.factor(substr(data$DS02,1,3)),
         DS_03 = as.factor(substr(data$DS03,1,3)),
         DS_04 = as.factor(substr(data$DS04,1,3)),
         DS_05 = as.factor(substr(data$DS05,1,3)),
         DS_06 = as.factor(substr(data$DS06,1,3)),
         DS_07 = as.factor(substr(data$DS07,1,3)),
         DS_08 = as.factor(substr(data$DS08,1,3)),
         DS_09 = as.factor(substr(data$DS09,1,3)),
         DS_10 = as.factor(substr(data$DS10,1,3)),
         DS_11 = as.factor(substr(data$DS11,1,3)),
         DS_12 = as.factor(substr(data$DS12,1,3)))

rm(data)

data00$DIAG3 <- paste(data00$DP,data00$DS_01,data00$DS_02,data00$DS_03, 
                   data00$DS_04,data00$DS_05,data00$DS_06,data00$DS_07,
                   data00$DS_08,data00$DS_09,data00$DS_10,data00$DS_11,
                   data00$DS_12,
                   sep=", ")
data00$DIAG3 <- gsub("NA, ","",data00$DIAG3)
data00$DIAG3 <- gsub(", NA","",data00$DIAG3)
data00$DIAG3 <- sapply(data00$DIAG3, 
                    function(x) paste(unique(unlist(str_split(x,", "))), 
                                      collapse = ", "))
data00$DIAGSEK3 <- paste(data00$DS_01,data00$DS_02,data00$DS_03, 
                      data00$DS_04,data00$DS_05,data00$DS_06,
                      data00$DS_07,data00$DS_08,data00$DS_09,
                      data00$DS_10,data00$DS_11,data00$DS_12,
                      sep=", ")
data00$DIAGSEK3 <- gsub("NA, ","",data00$DIAGSEK3)
data00$DIAGSEK3 <- gsub(", NA","",data00$DIAGSEK3)
data00$DIAGSEK3 <- gsub("NA","",data00$DIAGSEK3)
data00$DIAGSEK3 <- sapply(data00$DIAGSEK3, 
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))

#===========================================================================

setwd("D://UR//ICD 10 & 9-CM//icd102010enMeta")

chapters <- read.delim2('chapters.txt', header = FALSE, sep = ";", dec = ".")

chapters_headings <- c('chapter_number','chapter_title')
names(chapters) <- chapters_headings
chapters$chapters <- paste(chapters$chapter_number, "-", 
                           chapters$chapter_title)
chapters <- chapters %>% select(chapter_number,chapters)

blocks <- read.delim2('blocks.txt', header = FALSE, sep = ";", dec = ".")
blocks_headings <- c('character_31a', 'character_31b', 'chapter_number',
                     'block_title')
names(blocks) <- blocks_headings
blocks$blocks <- paste(blocks$character_31a, "-", blocks$character_31b,
                       " ", blocks$block_title)
blocks <- blocks %>%
  select(character_31a,blocks)

codes <- read.delim2('codes.txt', header = FALSE, sep = ";", dec = ".")
codes_headings <- c('level_classification', 'place_classification_tree',
                    'terminal_node', 'chapter_number', 'character_31a',
                    'code_wo_dagger', 'code_wo_asterisk', 'code_wo_dot',
                    'title', 'reference_mortality_1', 'reference_mortality_2',
                    'reference_mortality_3', 'reference_mortality_4',
                    'reference_morbidity')
names(codes) <- codes_headings

codes <- codes %>%
  select(chapter_number,character_31a,code_wo_dot, title)

codes <- left_join(codes, chapters,
                   by = c("chapter_number"="chapter_number"),
                   all.x = TRUE) %>%
  select(-chapter_number)

codes <- left_join(codes, blocks,
                   by = c("character_31a"="character_31a"),
                   all.x = TRUE) %>%
  select(-character_31a)

data00$NmDU <- NULL
data00$NmDM <- NULL

library(data.table)
library(caret)

data01 <- preProcess(as.data.frame(data01))

data01 <- left_join(data00, codes,
                    by = c("DU"="code_wo_dot"),all.x = TRUE)

data01 <- data01 %>%
  rename(NmDU = title, ChapDU = chapters,
         DP = blocks, DiaU = codes)

data01 <- left_join(data01, codes,
                    by = c("DS01"="code_wo_dot"),all.x = TRUE) %>%
  select(-title) %>%
  rename(ChapDS01 = chapters,
         blocksDS01 = blocks, DS01 = codes)
data01$DS01 [is.na(data01$DS01)] <- ""

data01 <- left_join(data01, codes,
                    by = c("DS02"="code_wo_dot"),all.x = TRUE) %>%
  select(-title) %>%
  rename(ChapDS02 = chapters,
         blocksDS02 = blocks, DS02 = codes)
data01$DS02 [is.na(data01$DS02)] <- ""

#============================================================================

library(readxl)
setwd("D:/data gresik/UR/Data Sampel BPJS Kesehatan 2015-2020 v01/Metadata")

DIAGFKTP <- read_excel("Kode ICD10 untuk diagnosis FKTP Kapitasi.xlsx", 
                       sheet = "Sheet1", range = NULL, 
                       col_names = TRUE, col_types = NULL, na = "",
                       trim_ws = TRUE, skip = 0)

#Kode ICD10 untuk diagnosis FKTP Kapitasi.xlsx

data01 <- left_join(data00, DIAGFKTP,
                    by = c("DP"="ICD10_Code"),all.x = TRUE) %>%
  select(-DP, -FKP14) %>%
  rename(DP = ICD10_Text)

rm(data00)

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_01"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_01, -FKP14) %>%
  rename(DS_01 = ICD10_Text)
data01$DS_01 [is.na(data01$DS_01)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_02"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_02, -FKP14) %>%
  rename(DS_02 = ICD10_Text)
data01$DS_02 [is.na(data01$DS_02)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_03"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_03, -FKP14) %>%
  rename(DS_03 = ICD10_Text)
data01$DS_03 [is.na(data01$DS_03)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_04"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_04, -FKP14) %>%
  rename(DS_04 = ICD10_Text)
data01$DS_04 [is.na(data01$DS_04)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_05"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_05, -FKP14) %>%
  rename(DS_05 = ICD10_Text)
data01$DS_05 [is.na(data01$DS_05)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_06"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_06, -FKP14) %>%
  rename(DS_06 = ICD10_Text)
data01$DS_06 [is.na(data01$DS_06)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_07"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_07, -FKP14) %>%
  rename(DS_07 = ICD10_Text)
data01$DS_07 [is.na(data01$DS_07)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_08"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_08, -FKP14) %>%
  rename(DS_08 = ICD10_Text)
data01$DS_08 [is.na(data01$DS_08)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_09"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_09, -FKP14) %>%
  rename(DS_09 = ICD10_Text)
data01$DS_09 [is.na(data01$DS_09)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_10"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_10, -FKP14) %>%
  rename(DS_10 = ICD10_Text)
data01$DS_10 [is.na(data01$DS_10)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_11"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_11, -FKP14) %>%
  rename(DS_11 = ICD10_Text)
data01$DS_11 [is.na(data01$DS_11)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_12"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_12, -FKP14) %>%
  rename(DS_12 = ICD10_Text)
data01$DS_12 [is.na(data01$DS_12)] <- ""

PROC <- read.csv2("sub chapter proc.csv", 
                  colClasses=c("factor", "character", "character"))
glimpse(PROC)

data02 <- data01 %>%
  mutate(Proc_01 = as.factor(substr(data01$Proc01,1,2)),
         Proc_02 = as.factor(substr(data01$Proc02,1,2)),
         Proc_03 = as.factor(substr(data01$Proc03,1,2)),
         Proc_04 = as.factor(substr(data01$Proc04,1,2)),
         Proc_05 = as.factor(substr(data01$Proc05,1,2)),
         Proc_06 = as.factor(substr(data01$Proc06,1,2)),
         Proc_07 = as.factor(substr(data01$Proc07,1,2)),
         Proc_08 = as.factor(substr(data01$Proc08,1,2)),
         Proc_09 = as.factor(substr(data01$Proc09,1,2)),
         Proc_10 = as.factor(substr(data01$Proc10,1,2)),
         Proc_11 = as.factor(substr(data01$Proc11,1,2)))

rm(data01)

data03 <- left_join(data02, PROC,
                    by = c("Proc_01"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_01, -Chap.Proc) %>%
  rename(Proc_01 = Sub.Chap.Proc)

rm(data02)

data03$Proc_01 [is.na(data03$Proc_01)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_02"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_02, -Chap.Proc) %>%
  rename(Proc_02 = Sub.Chap.Proc)
data03$Proc_02 [is.na(data03$Proc_02)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_03"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_03, -Chap.Proc) %>%
  rename(Proc_03 = Sub.Chap.Proc)
data03$Proc_03 [is.na(data03$Proc_03)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_04"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_04, -Chap.Proc) %>%
  rename(Proc_04 = Sub.Chap.Proc)
data03$Proc_04 [is.na(data03$Proc_04)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_05"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_05, -Chap.Proc) %>%
  rename(Proc_05 = Sub.Chap.Proc)
data03$Proc_05 [is.na(data03$Proc_05)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_06"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_06, -Chap.Proc) %>%
  rename(Proc_06 = Sub.Chap.Proc)
data03$Proc_06 [is.na(data03$Proc_06)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_07"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_07, -Chap.Proc) %>%
  rename(Proc_07 = Sub.Chap.Proc)
data03$Proc_07 [is.na(data03$Proc_07)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_08"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_08, -Chap.Proc) %>%
  rename(Proc_08 = Sub.Chap.Proc)
data03$Proc_08 [is.na(data03$Proc_08)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_09"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_09, -Chap.Proc) %>%
  rename(Proc_09 = Sub.Chap.Proc)
data03$Proc_09 [is.na(data03$Proc_09)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_10"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_10, -Chap.Proc) %>%
  rename(Proc_10 = Sub.Chap.Proc)
data03$Proc_10 [is.na(data03$Proc_10)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_11"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_11, -Chap.Proc) %>%
  rename(Proc_11 = Sub.Chap.Proc)
data03$Proc_11 [is.na(data03$Proc_11)] <- ""

PROC2 <- read.csv2("Chapter Procedure.csv", 
                  colClasses=c("factor", "character",
                               "character", "character"))

data04 <- data03 %>%
  mutate(Proc_001 = as.factor(substr(data03$Proc01,1,3)),
         Proc_002 = as.factor(substr(data03$Proc02,1,3)),
         Proc_003 = as.factor(substr(data03$Proc03,1,3)),
         Proc_004 = as.factor(substr(data03$Proc04,1,3)),
         Proc_005 = as.factor(substr(data03$Proc05,1,3)),
         Proc_006 = as.factor(substr(data03$Proc06,1,3)),
         Proc_007 = as.factor(substr(data03$Proc07,1,3)),
         Proc_008 = as.factor(substr(data03$Proc08,1,3)),
         Proc_009 = as.factor(substr(data03$Proc09,1,3)),
         Proc_010 = as.factor(substr(data03$Proc10,1,3)),
         Proc_011 = as.factor(substr(data03$Proc11,1,3)))

rm(data03)

data04$PROC3 <- paste(data04$Proc_001,data04$Proc_002,data04$Proc_003,data04$Proc_004,
                      data04$Proc_005,data04$Proc_006,data04$Proc_007,data04$Proc_008,
                      data04$Proc_009,data04$Proc_010,data04$Proc_011,sep=", ")
data04$PROC3 <- gsub("NA, ","",data04$PROC3)
data04$PROC3 <- gsub(", NA","",data04$PROC3)
data04$PROC3 <- gsub("NA","",data04$PROC3)
data04$PROC3 <- sapply(data04$PROC3, 
                     function(x) paste(unique(unlist(str_split(x,", "))), 
                                       collapse = ", "))

data05 <- left_join(data04, PROC2,
                    by = c("Proc_001"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_001, -Prosedur, -Chap.Proc) %>%
  rename(Proc_001 = Sub.Chap.Proc)

rm(data04)

data05$Proc_001 [is.na(data05$Proc_001)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_002"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_002, -Prosedur, -Chap.Proc) %>%
  rename(Proc_002 = Sub.Chap.Proc)
data05$Proc_002 [is.na(data05$Proc_002)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_003"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_003, -Prosedur, -Chap.Proc) %>%
  rename(Proc_003 = Sub.Chap.Proc)
data05$Proc_003 [is.na(data05$Proc_003)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_004"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_004, -Prosedur, -Chap.Proc) %>%
  rename(Proc_004 = Sub.Chap.Proc)
data05$Proc_004 [is.na(data05$Proc_004)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_005"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_005, -Prosedur, -Chap.Proc) %>%
  rename(Proc_005 = Sub.Chap.Proc)
data05$Proc_005 [is.na(data05$Proc_005)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_006"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_006, -Prosedur, -Chap.Proc) %>%
  rename(Proc_006 = Sub.Chap.Proc)
data05$Proc_006 [is.na(data05$Proc_006)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_007"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_007, -Prosedur, -Chap.Proc) %>%
  rename(Proc_007 = Sub.Chap.Proc)
data05$Proc_007 [is.na(data05$Proc_007)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_008"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_008, -Prosedur, -Chap.Proc) %>%
  rename(Proc_008 = Sub.Chap.Proc)
data05$Proc_008 [is.na(data05$Proc_008)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_009"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_009, -Prosedur, -Chap.Proc) %>%
  rename(Proc_009 = Sub.Chap.Proc)
data05$Proc_009 [is.na(data05$Proc_009)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_010"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_010, -Prosedur, -Chap.Proc) %>%
  rename(Proc_010 = Sub.Chap.Proc)
data05$Proc_010 [is.na(data05$Proc_010)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_011"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_011, -Prosedur, -Chap.Proc) %>%
  rename(Proc_011 = Sub.Chap.Proc)
data05$Proc_011 [is.na(data05$Proc_011)] <- ""

#LALI
data05 <- data05 %>%
  mutate(DIAGP = as.factor(substr(data05$DU,1,3)))

#==========================================================================
dataklaim <- data05 %>%
  select(Nokapst,Jkpst,Segmen,kdpst,pisa,Kelashak,Umur,
         Rangeumur,Nmdati2Terdaftar,Kdppkterdaftar,Nmppkterdaftar,
         Kdpks,Nmpks,prb,Nmdati2Perujuk,Kdppkperujuk,Nmppkperujuk,
         SEP,Nmdati2Layan,Kdppk,NmFKRTL,Pemilik,Kelasrsmenkes,Klsrawat,
         Nmtkp,Prvtkp,interval,Poli,DM,NmDM,Tglpelayanan,Start,End,
         Tglreg,Tglstjkeu,CBG,Kdinacbgs,CMG,Nmcmg,Nminacbgs,tipe,Spec,
         Sevel,DIAG,PROC,DU,DP,NmDU,DS,Noreg,DS01,DS_01,DS02,DS_02,DS03,
         DS_03,DS04,DS_04,DS05,DS_05,DS06,DS_06,DS07,DS_07,DS08,DS_08,
         DS09,DS_09,DS10,DS_10,DS11,DS_11,DS12,DS_12,DIAGP,DIAG3,DIAGSEK,
         DIAGSEK3,Jmlproc,PROC3,Procedure,Proc01,Proc_01,Proc02,Proc_02,
         Proc03,Proc_03,Proc04,Proc_04,Proc05,Proc_05,Proc06,Proc_06,
         Proc07,Proc_07,Proc08,Proc_08,Proc09,Proc_09,Proc10,Proc_10,
         Proc11,Proc_11,Proc_001,Proc_002,Proc_003,Proc_004,Proc_005,
         Proc_006,Proc_007,Proc_008,Proc_009,Proc_010,Proc_011,DPJP,
         Nmkronis,Nmkatastrofik,Kdjnsplg,LOS,Tarif,BiayaRS,Biaya,
         maxcase,maxuc)

dataklaim <- dataklaim[order(dataklaim$Nokapst, dataklaim$End),]
rm(data05)

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")
save(dataklaim, file="dataklaim.rda")

#=================================================================
#--------------------------LEFT JOIN------------------------------
#=================================================================

#4470 - AV shunt RJTL
## SUBSET
data_Q5 <- data %>%
  subset(Nmtkp == "RITL")


data_01 <- data_Q5 %>% 
  arrange(Nokapst, Start, End) %>% 
  group_by(Nokapst) %>%
  mutate(LOS = 1 + difftime(End, Start, units = 'days')) %>%
  mutate(laggedTimeElapsed = difftime(Start, lag(End),
                                      units = 'days'))

data_01 = data_01 %>% rename(DPJP = Namadpjp01)

#DATA_02 = koding variabel
data_02 <- data_01 %>%
  select(Nokapst, Tglpelayanan, Start, End, laggedTimeElapsed, Jkpst, Segmen,
         Kelashak, Umur, SEP, NmFKRTL, Kelasrsmenkes, Klsrawat, Nmtkp, Poli,
         CBG, CMG, tipe, Spec, Sevel, DU, NmDU, DS, DS01, DS02, DS03, DS04, 
         DS05, DS06, DS07, DS08, DS09, DS10, Procedure, Proc01, Proc02, 
         Proc03, Proc04, Proc05, Proc06, Proc07, Proc08, Proc09, Proc10, 
         Proc11, DPJP, Nmkatastrofik, Kdjnsplg, BiayaRS, Biaya, LOS)

#Transform Jkpst menjadi 1&0 --> 0= berarti Laki-laki, 1= berarti Perempuan
data_02$Jkpst[data_02$Jkpst=="Laki-laki"] <- "0"
data_02$Jkpst[data_02$Jkpst=="Perempuan"] <- "1"

#Transform Nmtkp menjadi 1&2 --> 1= berarti RITL, 2= berarti RJTL
data_02$Nmtkp[data_02$Nmtkp=="RITL"] <- "1"
data_02$Nmtkp[data_02$Nmtkp=="RJTL"] <- "2"

#Transform Segmen menjadi 1&0 --> 0= berarti PBI, 1= berarti Non-PBI
data_02$Segmen[data_02$Segmen=="PBI APBN"| 
                 data_02$Segmen=="PBI APBD"]<- "0"
data_02$Segmen[data_02$Segmen=="BP" |
                 data_02$Segmen=="PBPU" |
                 data_02$Segmen=="PPU BU" |
                 data_02$Segmen=="PPU PN" ] <- "1"

#Transform Kelashak menjadi 1,2,3
data_02$Kelashak[data_02$Kelashak=="Kelas I"] <- "1"
data_02$Kelashak[data_02$Kelashak=="Kelas II"] <- "2"
data_02$Kelashak[data_02$Kelashak=="Kelas III"] <- "3"

#Transform Klsrawat menjadi 1,2,3
data_02$Klsrawat[data_02$Klsrawat=="Kelas I"] <- "1"
data_02$Klsrawat[data_02$Klsrawat=="Kelas II"] <- "2"
data_02$Klsrawat[data_02$Klsrawat=="Kelas III"] <- "3"

glimpse(data_02)


#DATA_03 = Gabung DIAG & PROC
#remove empty cell with NA
data_03 <- na_if(data_02, "")
data_03$DIAG <- paste(data_03$DU,data_03$DS01,data_03$DS02,data_03$DS03, 
                      data_03$DS04,data_03$DS05,data_03$DS06,data_03$DS07, 
                      data_03$DS08,data_03$DS09, sep=", ")
data_03$DIAG <- gsub("NA, ","",data_03$DIAG)
data_03$DIAG <- gsub(", NA","",data_03$DIAG)
data_03$DIAG <- sapply(data_03$DIAG, 
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))
#nanti dihapus yaaa..
data_03$DIAG3 <- paste(data_03$DP,data_03$DS_01,data_03$DS_02,data_03$DS_03, 
                       data_03$DS_04,data_03$DS_05,data_03$DS_06,data_03$DS_07, 
                       data_03$DS_08,data_03$DS_09, sep=", ")
data_03$DIAG3 <- gsub("NA, ","",data_03$DIAG3)
data_03$DIAG3 <- gsub(", NA","",data_03$DIAG3)
data_03$DIAG3 <- sapply(data_03$DIAG3, 
                        function(x) paste(unique(unlist(str_split(x,", "))), 
                                          collapse = ", "))


data_03$PROC <- paste(data_03$Proc01,data_03$Proc02,data_03$Proc03,data_03$Proc04,
                      data_03$Proc05,sep=", ")
data_03$PROC <- gsub("NA, ","",data_03$PROC)
data_03$PROC <- gsub(", NA","",data_03$PROC)
data_03$PROC <- gsub("NA","",data_03$PROC)
data_03$PROC <- sapply(data_03$PROC, 
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))
#nanti dihapus yaaa..
data_03$PROC2 <- paste(data_03$Proc_01,data_03$Proc_02,data_03$Proc_03,data_03$Proc_04,
                       data_03$Proc_05,sep=", ")
data_03$PROC2 <- gsub("NA, ","",data_03$PROC2)
data_03$PROC2 <- gsub(", NA","",data_03$PROC2)
data_03$PROC2 <- gsub("NA","",data_03$PROC2)
data_03$PROC2 <- sapply(data_03$PROC2, 
                        function(x) paste(unique(unlist(str_split(x,", "))), 
                                          collapse = ", "))

data_03 <- na_if(data_03, "")

colnames(data_03)

SEP_DIAG <- data_03 %>% 
  select(SEP, DU, DS01, DS02, DS03, DS04, DS05, DS06, DS07, DS08, DS09,
         DP, DS_01, DS_02, DS_03, DS_04, DS_05, DS_06, DS_07, DS_08, DS_09,
         Proc01, Proc02, Proc03, Proc04, Proc05, Proc_01, Proc_02, Proc_03,
         Proc_04, Proc_05)

SEP_DIAG01 <- SEP_DIAG %>% 
  select(SEP, DU, DS01, DP, DS_01) %>% 
  rename(DS = DS01, DS3 = DS_01) %>%
  na.omit()

SEP_DIAG02 <- SEP_DIAG %>% 
  select(SEP, DU, DS02, DP, DS_02) %>% 
  rename(DS = DS02, DS3 = DS_02) %>%
  na.omit()

SEP_DIAG03 <- SEP_DIAG %>% 
  select(SEP, DU, DS03, DP, DS_03) %>% 
  rename(DS = DS03, DS3 = DS_03) %>%
  na.omit()

SEP_DIAG04 <- SEP_DIAG %>% 
  select(SEP, DU, DS04, DP, DS_04) %>% 
  rename(DS = DS04, DS3 = DS_04) %>%
  na.omit()

SEP_DIAG05 <- SEP_DIAG %>% 
  select(SEP, DU, DS05, DP, DS_05) %>% 
  rename(DS = DS05, DS3 = DS_05) %>%
  na.omit()

SEP_DIAG06 <- SEP_DIAG %>% 
  select(SEP, DU, DS06, DP, DS_06) %>% 
  rename(DS = DS06, DS3 = DS_06) %>%
  na.omit()

SEP_DIAG07 <- SEP_DIAG %>% 
  select(SEP, DU, DS07, DP, DS_07) %>% 
  rename(DS = DS07, DS3 = DS_07) %>%
  na.omit()

SEP_DIAG08 <- SEP_DIAG %>% 
  select(SEP, DU, DS08, DP, DS_08) %>% 
  rename(DS = DS08, DS3 = DS_08) %>%
  na.omit()

SEP_DIAG09 <- SEP_DIAG %>% 
  select(SEP, DU, DS09, DP, DS_09) %>% 
  rename(DS = DS09, DS3 = DS_09) %>%
  na.omit()

SEP_PROC01 <- SEP_DIAG %>% 
  select(SEP, DU, Proc01, DP, Proc_01) %>% 
  rename(PROC = Proc01, PROC2 = Proc_01) %>%
  na.omit()

SEP_PROC02 <- SEP_DIAG %>% 
  select(SEP, DU, Proc02, DP, Proc_02) %>% 
  rename(PROC = Proc02, PROC2 = Proc_02) %>%
  na.omit()

SEP_PROC03 <- SEP_DIAG %>% 
  select(SEP, DU, Proc03, DP, Proc_03) %>% 
  rename(PROC = Proc03, PROC2 = Proc_03) %>%
  na.omit()

SEP_PROC04 <- SEP_DIAG %>% 
  select(SEP, DU, Proc04, DP, Proc_04) %>% 
  rename(PROC = Proc04, PROC2 = Proc_04) %>%
  na.omit()

SEP_PROC05 <- SEP_DIAG %>% 
  select(SEP, DU, Proc05, DP, Proc_05) %>% 
  rename(PROC = Proc05, PROC2 = Proc_05) %>%
  na.omit()

SEP_DIAG00 <- rbind(SEP_DIAG01, SEP_DIAG02, SEP_DIAG03, SEP_DIAG04, SEP_DIAG05,
                    SEP_DIAG06, SEP_DIAG07, SEP_DIAG08, SEP_DIAG09) %>%
  arrange(SEP)

SEP_PROC00 <- rbind(SEP_PROC01, SEP_PROC02, SEP_PROC03, SEP_PROC04, SEP_PROC05) %>%
  arrange(SEP)

df <- merge(x = SEP_DIAG00, y = SEP_PROC00, by = c( "SEP","DU", "DP"), 
            all = TRUE) %>%
  arrange(SEP)
save(df, file = "dfdiagproc.rda")

save(data_03, file = "Q455.rda")

data_03 <- data_03 %>%
  select(Nokapst, Tglpelayanan, Start, End, Jkpst, Segmen, Kelashak, SEP, 
         NmFKRTL, Kelasrsmenkes, Klsrawat, CBG, Sevel, DIAG, PROC, DPJP, 
         Kdjnsplg, BiayaRS, Biaya, LOS)

data_03 <- data_03 %>% 
  filter(str_detect(PROC, c("4470")))


%>%
  subset(Nmtkp == 1)

data_03 <- data_03 %>% 
  filter(str_detect(DIAG, c("P030|P031|P032|P033|P034|P035|P036"))) %>%
  subset(Klsrawat != 3)

glimpse(data_03)
colnames(data_03)

data_03 <- data_03 %>%
  filter(str_detect(DIAG, c("B342")))


%>%
  subset(Kdjnsplg == 2)

data_03 <- subset(data_03, data_03$Tglpelayanan >= "2020-03-01")

%>%
  filter(Kdjnsplg == 2)

write.csv(data, "D://dataextractdumai//buban bojonegoro//2014-2021//data_lap_kewil.csv",
          na="", row.names = FALSE)

df_DS = read_csv("data_lap_kewil.csv") %>% select(SEP, DS)

library(splitstackshape)
df_DS <- concat.split(df_DS, "DS", ";")
df_DS$DS01 <- as.character(trimws(df_DS$DS_01, "both"))
df_DS$DS02 <- as.character(trimws(df_DS$DS_02, "both"))
df_DS$DS03 <- as.character(trimws(df_DS$DS_03, "both"))
df_DS$DS04 <- as.character(trimws(df_DS$DS_04, "both"))
df_DS$DS05 <- as.character(trimws(df_DS$DS_05, "both"))
df_DS$DS06 <- as.character(trimws(df_DS$DS_06, "both"))
df_DS$DS07 <- as.character(trimws(df_DS$DS_07, "both"))
df_DS$DS08 <- as.character(trimws(df_DS$DS_08, "both"))
df_DS$DS09 <- as.character(trimws(df_DS$DS_09, "both"))
df_DS$DS10 <- as.character(trimws(df_DS$DS_10, "both"))
df_DS <- df_DS %>% select(-2:-12)

df.DS01 = df_DS %>% select(SEP, DS01)
df.DS01 = df.DS01 %>% rename(DS = DS01)
df.DS01 = na.omit(df.DS01)
df.DS02 = df_DS %>% select(SEP, DS02)
df.DS02 = df.DS02 %>% rename(DS = DS02)
df.DS02 = na.omit(df.DS02)
df.DS03 = df_DS %>% select(SEP, DS03)
df.DS03 = df.DS03 %>% rename(DS = DS03)
df.DS03 = na.omit(df.DS03)
df.DS04 = df_DS %>% select(SEP, DS04)
df.DS04 = df.DS04 %>% rename(DS = DS04)
df.DS04 = na.omit(df.DS04)
df.DS05 = df_DS %>% select(SEP, DS05)
df.DS05 = df.DS05 %>% rename(DS = DS05)
df.DS05 = na.omit(df.DS05)
df.DS06 = df_DS %>% select(SEP, DS06)
df.DS06 = df.DS06 %>% rename(DS = DS06)
df.DS06 = na.omit(df.DS06)
df.DS07 = df_DS %>% select(SEP, DS07)
df.DS07 = df.DS07 %>% rename(DS = DS07)
df.DS07 = na.omit(df.DS07)
df.DS08 = df_DS %>% select(SEP, DS08)
df.DS08 = df.DS08 %>% rename(DS = DS08)
df.DS08 = na.omit(df.DS08)
df.DS09 = df_DS %>% select(SEP, DS09)
df.DS09 = df.DS09 %>% rename(DS = DS09)
df.DS09 = na.omit(df.DS09)
df.DS10 = df_DS %>% select(SEP, DS10)
df.DS10 = df.DS10 %>% rename(DS = DS10)
df.DS10 = na.omit(df.DS10)

df.DS <- rbind(df.DS01, df.DS02, df.DS03, df.DS04, df.DS05, df.DS06, df.DS07, 
               df.DS08, df.DS09, df.DS10)

write.csv(df.DS, "D://dataextractdumai//buban bojonegoro//2014-2021//DS.csv",
          na="", row.names = FALSE)

df_Proc = read_csv("data_lap_kewil.csv") %>% select(SEP, Procedure)
df_Proc <- concat.split(df_Proc, "Procedure", ";")

df_Proc$Proc01 <- as.character(trimws(df_Proc$Procedure_01, "both"))
df_Proc$Proc02 <- as.character(trimws(df_Proc$Procedure_02, "both"))
df_Proc$Proc03 <- as.character(trimws(df_Proc$Procedure_03, "both"))
df_Proc$Proc04 <- as.character(trimws(df_Proc$Procedure_04, "both"))
df_Proc$Proc05 <- as.character(trimws(df_Proc$Procedure_05, "both"))
df_Proc$Proc06 <- as.character(trimws(df_Proc$Procedure_06, "both"))
df_Proc$Proc07 <- as.character(trimws(df_Proc$Procedure_07, "both"))
df_Proc$Proc08 <- as.character(trimws(df_Proc$Procedure_08, "both"))
df_Proc$Proc09 <- as.character(trimws(df_Proc$Procedure_09, "both"))
df_Proc$Proc10 <- as.character(trimws(df_Proc$Procedure_10, "both"))
df_Proc$Proc11 <- as.character(trimws(df_Proc$Procedure_11, "both"))
df_Proc <- df_Proc %>% select(-2:-13)

df.Proc01 = df_Proc %>% select(SEP, Proc01)
df.Proc01 = df.Proc01 %>% rename(Proc = Proc01)
df.Proc01 = na.omit(df.Proc01)
df.Proc02 = df_Proc %>% select(SEP, Proc02)
df.Proc02 = df.Proc02 %>% rename(Proc = Proc02)
df.Proc02 = na.omit(df.Proc02)
df.Proc03 = df_Proc %>% select(SEP, Proc03)
df.Proc03 = df.Proc03 %>% rename(Proc = Proc03)
df.Proc03 = na.omit(df.Proc03)
df.Proc04 = df_Proc %>% select(SEP, Proc04)
df.Proc04 = df.Proc04 %>% rename(Proc = Proc04)
df.Proc04 = na.omit(df.Proc04)
df.Proc05 = df_Proc %>% select(SEP, Proc05)
df.Proc05 = df.Proc05 %>% rename(Proc = Proc05)
df.Proc05 = na.omit(df.Proc05)
df.Proc06 = df_Proc %>% select(SEP, Proc06)
df.Proc06 = df.Proc06 %>% rename(Proc = Proc06)
df.Proc06 = na.omit(df.Proc06)
df.Proc07 = df_Proc %>% select(SEP, Proc07)
df.Proc07 = df.Proc07 %>% rename(Proc = Proc07)
df.Proc07 = na.omit(df.Proc07)
df.Proc08 = df_Proc %>% select(SEP, Proc08)
df.Proc08 = df.Proc08 %>% rename(Proc = Proc08)
df.Proc08 = na.omit(df.Proc08)
df.Proc09 = df_Proc %>% select(SEP, Proc09)
df.Proc09 = df.Proc09 %>% rename(Proc = Proc09)
df.Proc09 = na.omit(df.Proc09)
df.Proc10 = df_Proc %>% select(SEP, Proc10)
df.Proc10 = df.Proc10 %>% rename(Proc = Proc10)
df.Proc10 = na.omit(df.Proc10)
df.Proc11 = df_Proc %>% select(SEP, Proc11)
df.Proc11 = df.Proc11 %>% rename(Proc = Proc11)
df.Proc11 = na.omit(df.Proc11)