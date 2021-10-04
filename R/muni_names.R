library(geobr)
library(data.table)
library(ggplot2)
library(sf)

`%nin%` <- Negate(`%in%`)
`%nlike%` <- Negate(`%like%`)



########### Download data ------------------------------------------------------------------

states <- read_state()
munis <- read_municipal_seat()
setDT(munis)

########### Indigenous names ------------------------------------------------------------------

###### prefix - starting with
pre <- c( 'Aba', 'Aca', 'Aça', 'Açu', 'Acre', 'Afu', 'Anapu', 'Anicuns', 'Api', 'Apu',
          'Assa', 'Ara', 'Ari', 
          'Aru', 'Ata', 'Ava', 'Anh',
          'Bacab', 'Baraú', 'Biri', 'Braú', 'Bora', 'Borá', 'Bore', 'Botu', 'Buri',
          'Caa', 'Cai', 'Caí', 'Caja', 'Caju', 'Catu', 'Capivari', 'Cara', 'Cari', 
          'Carna', 'Chapecó', 'Crix', 'Coru', 'Cote', 'Cui', 'Curi', 'Coit', 'Coiv',
          'Camo', 'Cambu',
          'Emb', 'Ere',
          'Goiá', 'Goia', 'Gru', 'Gua', 'Guira', 'Guiri', 'Guri', 'Guru', 
          'Ia', 'Ic', 'Iba', 'Ibe', 'Ibi', 'Ibo', 'Iga', 'Igua', 'Inh', 'Ira',
          'Ipa', 'Ipe', 'Inú', 'Itá', 'Ipi', 'Iti', 'Ipu', 'Ira', 'Ire', 'Iri',
          'Iru', 'Ita', 'Itpo', 'Itu', 'Ijuí',
          'Ingá', 'Inga', 'Inda',
          'Jabo', 'Jaca', 'Jacu', 'Jagua', 'Jand', 'Jape', 'Jari', 'Jat', 'Jenipapo', 'Jequ', 'Juc', 'Jussa', 'Juru', 'Jund',
          'Maca', 'Macu', 'Mana', 'Mand', 'Mamb', 'Mara', 'Massa', 'Mauá', 'Maué', 'Matu', 'Mira', 'Morun', 'Moji', 'Muri',
          'Mulu', 'Mucu',
          'Nha',
          'Oca', 'Oro', 'Orix',
          'Pareci', 'Parna', 'Pita', 'Poti',
          'Paca', 'Para', 'Pari', 'Pind', 'Pira', 'Piri', 
          'Quix', 'Quij', 'Quipa',
          'Rora', 
          'Saba', 'Sap', 'Sape', 'Sapi', 'Sapu', 'Saqua', 'Suma', 'Sumé',
          'Taba', 'Tabi', 'Tapi', 'Tai', 'Taca', 'Tacu', 'Taci', 'Tau', 'Tagua', 'Tama', 'Taqua', 'Tapi', 'Tape', 'Tapu',
          'Tara', 'Tau', 'Trac', 
          'Tib', 'Tie', 'Timb', 'Tiju', 'Toca', 'Tori', 'Tucan', 'Tucu',  'Tupa', 'Tupã', 'Tupi', 
          'Ua', 'Uba', 'Ube', 'Ubi', 'Uira', 'Una', 'Umbu', 'Uru',
          'Voto', 'Votu',
          'Xax', 'Xam', 'Xavantina', 'Xingu', 'Xique',
          'Anamã', 'Apodi', 'Utinga', 'Itinga', 'Ubá', 'Taió', 'Toropi', 'Poá',
          'Nipoã', 'Muqui', 'Muçum', 'Motuca', 'Mossoró', 'Moju', 'Japi', 'Ivaí',
          'Ipê', 'Iepê', 'Catanduvas', 'Cataguases', 'Sooretama', 'Toritama', 
          'Canindé', 'Camaragibe', 'Camacan', 'Calumbi', 'Caculé', 'Atibaia'
          )

###### sufixo - ending with
suf <- c('aba', 'abã', 'abi', 'abu', 'aca', 'aci', 'açu', 'acy', 'ahy', 'aiá', 'araí', 'agé',
         'ajás', 'ajá', 'aju', 'anã', 'apã', 'apá', 'aná', 'aú', 'andi', 'anga', 'angui', 'andu', 'amã', 'ambi', 'aji', 'apá', 'ará', 'ari', 'araó', 
         'aru', 'atã', 'assu', 'atá', 'até', 'atu', 'aty', 'auá', 'aroba', 'aeté',
         'atinga', 'itinga', 'utinga',
         'bai', 'baí',
         'çara', 'caia', 'caré', 'catu', 'cema', 'çuí', 
         'daí', 'daré', 
         'etinga', 'ema', 'eri', 'eua', 'erê', 'etá', 'etama',
         'gaba', 'guá', 'guaí', 'guaia', 
         'hy', 'iré', 'iú', 'iju', 'ípe', 'ixi', 'izal', 'itá',
         'riri',
         'iá', 'iaí', 'iaba', 'iara', 'iba', 'íba', 'íbe', 'ibé', 'ína', 'íra', 'iri', 
         'ité', 'iti', 'itu', 'ipu',
         'arama', 'erama', 'irama', 'orama', 'urama',
         'jaí', 'juba', 'juí', 
         'laia', 'mbé', 'mbu', 'mirim', 'Mirim',
         'oara', 'oba', 'obá', 'obé', 'obi', 'oga', 'orã', 'orá', 'oré', 'oti',
         'pira', 'ponga', 'pora', 'porá', 'puru',
         'quara', 'quira',
         'raí', 'raré', 'maré',
         'tã', 'taba', 'tama', 'taí', 'tiba', 'tuba', 'tum',
         'uana', 'uba', 'úba', 'uca', 'ucá', 'umã', 'uva', 'úva', 'ubim', 'uaia', 'uçu', 'uçú', 
         'ussu', 'uí', 'uiú', 'ujá', 'umbi',
         'uá', 'úna', 'upi', 'urá', 'uré', 'uri', 'uru', 'uti', 
         'xim'
         )



# at the start
munis[ grepl('Paraíso',  name_muni ) ]$name_muni

# at the end
munis[ grepl('aná\\b',  name_muni ) ]$name_muni |> sort()



# build unifying search term for muni names starting in pre
  search_pre <- paste0(pre, collapse = '|')

# build unifying search term for muni names ending in suf
  search_suf <- paste0(suf, collapse = '\\b|')
  search_suf <- paste0(search_suf, '\\b')
  
# get municipality codes starting in pre OR ending in suf
code_suf <- munis[ grepl(search_suf,  name_muni ) ]$code_muni
code_pre <- munis[ grepl(search_pre,  name_muni ) ]$code_muni
code_ind <- unique(c(code_pre, code_suf))

# avoid confusion, removing dubious city names
ind_conf <- c('Paraíso')
ind_conf <- munis[ name_muni %like% ind_conf]$code_muni
code_ind <- code_ind[code_ind %nin% ind_conf]

length(code_ind)
#> 1886



########### Religious names ------------------------------------------------------------------

# all search terms
rel <- c(  'Abadi' 
         , 'Aparecida'
         , 'Belém'
         , 'Bonfim'
         , 'Canaã'
         , 'Cruz'
         , 'Cruzes'
         , 'Divin'
         , 'Frei'
         , 'Jericó'
         , 'Jesus'
         , 'Jordão'
         , 'Maria'
         , 'Missões'
         , 'Nazaré'
         , 'Nossa'
         , 'Padre'
         , 'Paraíso'
         , 'Pio'
         , 'Salvador'
         , 'Santa'
         , 'Santo'
         , 'São'
         , 'Trindade'
         )

# build unifying search term
search_rel <- paste0(rel, collapse = '|')

# get code of all municipalities with religious names
code_rel <- subset(munis, name_muni %like% search_rel)$code_muni
length(code_rel)
#> 805


########### other name origins ------------------------------------------------------------------

### military
# coronel, sargento, general, capitão

### nature
# cachoeira, lagoa, pedra, rocha?, Riach, Bebedouro, Cafelandia, Mata
# Alto


########### Recode ------------------------------------------------------------------

munis[, name_ind := fifelse(code_muni %in% code_ind, 1, 0) ]
munis[, name_rel := fifelse(code_muni %in% code_rel, 1, 0) ]
munis[, name_indrel := fifelse(name_rel==1 & name_ind==1, 1, 0) ]

munis[, name_origen := fcase(name_indrel==1, 'Indig-Relig',
                             name_rel ==1, "Relig.", 
                             name_ind ==1, "Indig.",
                             default = 'Other')]
                                 

# check
table(munis$name_origen)
a <- subset(munis, name_origen =='Indig-Relig')
b <- subset(munis, name_origen =='Indig.')
c <- subset(munis, name_origen =='Other')



# back to sf
munis_sf <- st_sf(munis)

########### Plot ------------------------------------------------------------------

temp_fig <- ggplot() +
            geom_sf(data=states, color='gray', fill='gray95', size=.2) +
            # geom_sf(data=munis_sf, color='gray', size=.5, alpha=.5) +
            geom_sf(data=munis_sf, aes(color=name_origen), size=.1, alpha=.5) +
            facet_wrap(.~name_origen) +
            theme_void() +
            theme(legend.position = "none")

ggsave(temp_fig, filename = './figures/name_origins_municipalities_br.png',
       width = 16, height = 16, units = 'cm', dpi=200)

