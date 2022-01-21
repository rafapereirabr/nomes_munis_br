library(geobr)
library(data.table)
library(ggplot2)
library(ggpointdensity)
library(viridisLite)
library(sfheaders)
library(sf)

`%nin%` <- Negate(`%in%`)
`%nlike%` <- Negate(`%like%`)



########### Download data ------------------------------------------------------------------

states <- read_state()
munis <- read_municipal_seat()
setDT(munis)


########### Load name identifiers and search terms ------------------------------------------------------------------


########### Indigenous names ------------------------------------------------------------------

###### prefix - starting with
pre <- c( 'Aba', 'Aca', 'Aça', 'Açu', 'Acre', 'Afu', 'Anapu', 'Anicuns', 'Api', 'Apu',
          'Assa', 'Ara', 'Ari', 
          'Aru', 'Ata', 'Ava', 'Anh',
          'Bacab', 'Baraú', 'Biri', 'Braú', 'Bora', 'Borá', 'Bore', 'Botu', 'Buri',
          'Caa', 'Cai', 'Caí', 'Caja', 'Caju', 'Catu', 'Capivari', 'Cara', 'Cari', 'Caru',
          'Carna', 'Chapecó', 'Crix', 'Coru', 'Cote', 'Cui', 'Curi', 'Curu', 'Coit', 'Coiv',
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
          'Xax', 'Xam', 
          # full names
          'Xavantina', 'Xingu', 'Xique',
          'Anamã', 'Apodi', 'Utinga', 'Itinga', 'Ubá', 'Taió', 'Toropi', 'Poá',
          'Nipoã', 'Muqui', 'Muçum', 'Motuca', 'Mossoró', 'Moju', 'Japi', 'Ivaí',
          'Ipê', 'Iepê', 'Catanduvas', 'Cataguases', 'Sooretama', 'Toritama', 
          'Canindé', 'Camaragibe', 'Camacan', 'Calumbi', 'Caculé', 'Atibaia',
          'Caxingó', 'Sussuapara', 'Aquiraz', 'Codó', 'Bodó', 'Cubati', 'Caetés',
          'Cabrobó', 'Camamu', 'Aiur', 'Aimorés', 'Baependi', 'Coromandel', 'Tefé',
          'Cuparaque', 'Nanuque', 'Naque', 'Piumhi', 'Osasco', 'Cacequi', 'Jaquirana')

###### sufixo - ending with
suf <- c('aba', 'abã', 'abi', 'abu', 'aca', 'açá', 'aci', 'açu', 'acy', 'ahy', 'aiá', 'araí', 'agé',
         'ajás', 'ajá', 'aju', 'anã', 'apã', 'apá', 'aná', 'aú', 'andi', 'anga', 'angui', 'andu', 'amã', 'ambi', 'aji', 'apá', 'ará', 'ari', 'araó', 
         'aru', 'atã', 'assu', 'atá', 'até', 'atu', 'aty', 'atiba', 'auá', 'aroba', 'aeté', 'ataia',
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
         'riú',
         'tã', 'taba', 'tama', 'taí', 'tiba', 'tuba', 'tum',
         'uana', 'uba', 'úba', 'uca', 'ucá', 'umã', 'uva', 'úva', 'ubim', 'uaia', 'uçu', 'uçú', 
         'ussu', 'uí', 'uiú', 'ujá', 'umbi', 'uoca',
         'uá', 'úna', 'upi', 'urá', 'uré', 'uri', 'uru', 'uti', 
         'xim'
)

# ## quick searches
# # at the start
# munis[ grepl('Caxi',  name_muni ) ]$name_muni
# 
# # at the end
# munis[ grepl('rupu\\b',  name_muni ) ]$name_muni |> sort()
# 


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
ind_conf <- c('Paraíso', 'Valparaíso')
ind_conf <- munis[grepl(paste(ind_conf, collapse='|'), name_muni)]$code_muni
code_ind <- code_ind[code_ind %nin% ind_conf]


length(code_ind)
#> 1914




########### Religious names ------------------------------------------------------------------

# all search terms
rel <- c(  'Abadi' 
           , 'Aparecida'
           , 'Belém'
           , 'Bonfim'
           , 'Canaã'
           , 'Capela', 'Capelinha'
           , 'Cristo'
           , 'Cruz'
           , 'Cruzes'
           , 'Divin'
           , 'Frei'
           , 'Galiléia'
           , 'Igrej'
           , 'Jericó'
           , 'Jesus'
           , 'Jordão'
           , 'Maria'
           , 'Mesquita'
           , 'Milagre'
           , 'Missões'
           , 'Natal'
           , 'Nazaré', 'Nazareno'
           , 'Nossa'
           , 'Oratório'
           , 'Padre'
           , 'Pastor'
           , 'Pastora'
           , 'Paraíso'
           , 'Pio'
           , 'Redentor' 
           , 'Romaria'
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
#> 827




########### Political name origins ------------------------------------------------------------------
#' !!!!! There many cities named after politicians. However, it is quite
#' hard to automatically differentiate cities named after politicians from cities named
#' after other professions.

pol1 <- c('Presidente', 'Senador', 'Senadora', 
          'Deputado', 'Deputada', 
          'Vereador', 'Vereadora',
          'Prefeito', 'Prefeita',
          'Juiz', 'Juiza',
          'Embaixador', 'Embaixadora', 'Embaixatriz' )



# build unifying search term
search_pol1 <- paste0(pol1, collapse = '|')

# get code of all municipalities with religious names
code_pol1 <- subset(munis, name_muni %like% search_pol1)$code_muni


########### Military / nobility name origins  ------------------------------------------------------------------

pol <- c( 'Rei', 'Rainha', 'Barão',  'Baronesa', 
          'Conde', 'Condeza', 'Visconde', 
          'Duque', 'Duquesa', 
          'Imperador', 'Imperatriz',
          
          'Marechal', 'General', 'Coronel', 
          'Almirante',
          'Sargento', 'capitão', 
          'Comandante', 'Tenente', 'Soldado'
        )

subset(munis, name_muni %like% 'Dom ')

# build unifying search term
search_pol <- paste0(pol, collapse = '|')

# get code of all municipalities with religious names
code_pol <- subset(munis, name_muni %like% search_pol)$code_muni
length(code_pol)
#> 51



########### Nature name origins ------------------------------------------------------------------

nat <- c(
  # physical geography ish
  'Alto', 'Alta', 'Areal',
  'Água', 'água',
  'Barreir','Barra', 'Barrinha',
  'Bahia', 'Baía' , 'Enseada', 'Cabo', 'Estreito', 'Bebedouro',
  'Cachoeir', 'Queda',
  'Morro', 'Morrinho', 'Monte', 'Montanha', 'Serra', 'Serrinha','Serrolândia',
  'Colina', 'Pico',
  'Pedra', 'Pedreira', 'Pedrinha', 'Rocha',
  'Rio', 'Riach', 'Ribeir', 'Brejo', 'Brejinho', 'Brejão','Brejões', 'Curralinho',
  'Córrego' , 'Lago', 'Alagoa', 'Fonte' ,
  'Mata', 'Matinha', 'Matão', 'Matões', 'Mato', 'Campo', 'Vargem', 'Vargi', 'Varjã',
  'Chapad',
  'Praia', 'Prainha',
  'Ilh',
  'Jardim',
  'Vista', 'Horizonte',
  'Monção',
  'Baixio',
  'Poço', 'Pocinhos',
  'Campina', 'Prado', 'Campestre',
  'Cating',
  'Sertão',
  'Várzea', 'Varzelân',
  'Vereda', 'Veredinha', 'Gramado' ,
  'Vazante',
  'Recife',
  'Serrita',
  'Mar ',
  'Maresia',
  'Vieira',
  'Videira',
  'Fundão',
  'Arroio',
  'Aurora',
  'Balneário' ,
  'Cerro' ,
  'Mormaço',
  'Morrinhos' ,
  'Sertão',
  'Duna',
  'Sol',
  'Lua',
  'Corguinho',
  'Rochedo',
  
  # trees and fruits
  'Angico',
  'Babaçu',
  'Cereje',
  'Cacau',
  'Espigã',
  'Espig',
  'Castanhei',
  'Cacoal',
  'Piment',
  'Seringu',
  'Castanh',
  'Figueir',
  'Oliveir',
  'Palmeir',
  'Pequi',
  'Axixá',
  'Cedral',
  'Mirinzal',
  'Pinheiro',
  'Rosári', 'Rosal',
  'Agricol', 'Agrolând', 'Agronôm',
  'Canavi',
  'Cafe', 'Café',
  'Açaí', 'Açail',
  'Cocal', 'Cocais',
  'Mangabeira',
  'Limoei',
  'Madeir',
  'Maripá',
  'alho', 'Alho',
  'Capinzal', 'Capim', 'Capinópolis',
  'Espinh',
  'Flor ', 'Flores' ,
  'Aroeiras',
  'Bananeir', 'Bananal',
  'Cedro',
  'Flores', 'Flora',
  'Jaqueira', 'Jaca',
  'Palmares',
  'Junqueiro',
  'Maribondo',
  'Laranjeiras',
  'Cipó',
  'Cocos',
  'Cana ',
  'Carvalho',
  'Araucária',
  'Folhas',
  'Floresta',
  'Frutal',
  'Laranja',
  'Lavra', 'Lavrinhas',
  'Limeira' ,
  'Lontra',
  'Paineira',
  'Palma', 'Palmó',
  'Pinheir',
  'Canelinha', 'Canela',
  'Imbuia',
  'Alecrim',
  'Arvore', 'Árvore',
  'Capão',
  'Cidreira',
  'Coqueiro',
  'Palmital',
  'Palmitos',
  'Pinhão',
  
  # animals and insects
  'Peixe',
  'Piranha',
  'Trairão',
  'Tartarug',
  'Bagre',
  'Colméia',
  'Formigu',
  'Coelho',
  'Formig',
  'Cascavel',
  'Granj',
  'Tubar',
  'Cutia',
  'Ananá',
  'Pium',
  'Raposa', 'Raposo',
  'Galinho',
  'Touro',
  'Pato',
  'Pombal',
  'Carneiro',
  'Cordeir',
  'Lagarto',
  'Bois',
  'Pinhão',
  'Andorinha',
  'Anta',
  'Gavião',
  'Cantagalo',
  'Cristais', 'Cristal',
  'Diamantina',
  'Águia', 
  'Papagaio',
  'Pavão',
  'Quati',
  'Cotia',
  'Cascavel',
  'Gavião',
  'Lontra',
  
  # Others
  'Adamantina',
  'Arco-Íris',
  'Areia',
  'Cristal',
  'Primavera',
  'Leite',
  'Barro',
  'Neves',
  'Ouro',
  'Prata', 'Pratinha',
  'Cristal',
  'Alvorada', 'Aurora',
  'Horizonte',
  'Pau ',
  'Terra', 'terra',
  'Hidro',
  'Salitre',
  'Saboeiro',
  'Cacimba',
  'Deserto',
  'Estância',
  'Sítio', 'Chácara', 'Chalé',
  'Bicas', 'Biquinhas',
  'Ponte',
  'Porteir',
  'Salinas',
  'Castelo',
  'Linhares',
  'Conch',
  'Óleo',
  'Palhoça',
  'Pedregulho',
  'Pinhalzinho',
  'Planalto',
  'Pontão',
  'Pontal',
  'Pontalinda',
  'Restinga',
  'Saltinho',
  'Salto',
  'Sertãozinho',
  'Urânia',
  'Céu' ,
  'Cerro'
  )




# build unifying search term
search_nat <- paste0(nat, collapse = '|')

# get code of all municipalities with religious names
code_nat <- subset(munis, name_muni %like% search_nat)$code_muni

subset(munis, name_muni %like% "Flora")$name_muni |> sort()
length(code_nat)
#>  1320


########### Classify municipality names ------------------------------------------------------------------

munis[, name_ind := fifelse(code_muni %in% code_ind, 'Ind', '') ]
munis[, name_rel := fifelse(code_muni %in% code_rel, 'Rel', '') ]
munis[, name_nat := fifelse(code_muni %in% code_nat, 'Nat', '') ]
munis[, name_origin := paste0(name_ind, name_rel, name_nat, collapse = '-'), by=code_muni]
munis[, name_origin := fifelse(name_origin == '', 'Other', name_origin) ]
head(munis)                      

                          

# check
table(munis$name_origin)
a <- subset(munis, name_origin %like% 'Ind')
b <- subset(munis, name_origin %like% 'Rel')
c <- subset(munis, name_origin %like% 'Nat')
d <- subset(munis, name_origin %like% 'Other')



########### numbers ------------------------------------------------------------------

# proportion of municipalities with indigenous names
nrow(subset(munis, name_origin %like% 'Ind')) / nrow(munis)

# proportion of municipalities with religious names
nrow(subset(munis, name_origin %like% 'Rel')) / nrow(munis)

# proportion of municipalities with religious names
nrow(subset(munis, name_origin %like% 'Nat')) / nrow(munis)

consolidated <- munis[, .(total = .N,
                            ind = sum(name_origin %like% 'Ind'),
                            rel = sum(name_origin %like% 'Rel'),
                          nat = sum(name_origin %like% 'Nat'),
                          oth = sum(name_origin %like% 'Other')
), by=abbrev_state ]

# add proportions
consolidated[, ind_prop := round( ind / total, digits = 2)]
consolidated[, rel_prop := round( rel / total, digits = 2)]
consolidated[, nat_prop := round( nat / total, digits = 2)]
consolidated[, oth_prop := round( oth / total, digits = 2)]

# sort
consolidated <- consolidated[order(ind_prop, rel_prop, nat_prop)]
consolidated




########### prepare data for Plot munis ------------------------------------------------------------------

# back to sf
munis_sf <- st_sf(munis)

# data frame
munis_df <- sfheaders::sf_to_df(munis,fill = T)




########### Plot munis ------------------------------------------------------------------

temp_fig <- ggplot() +
            geom_sf(data=states, color='gray', fill='gray95', size=.2) +
            # geom_sf(data=munis_sf, color='gray', size=.5, alpha=.5) +
            geom_sf(data=munis_sf, aes(color=name_origin), size=.1, alpha=.5) +
            facet_wrap(.~name_origin) +
            theme_void() +
            theme(legend.position = "none")

ggsave(temp_fig, filename = './figures/name_origins_municipalities_br.png',
       width = 16, height = 16, units = 'cm', dpi=200)


##### point density

temp_fig2 <-ggplot() +
            geom_sf(data=states, color='gray', fill='gray95', size=.2) +
            geom_pointdensity(data=munis_df, aes(x=x,y=y), size=.1, alpha=.5) +
            facet_wrap(.~name_origin) +
            scale_color_viridis_c() +
            # theme_minimal() +
            theme(legend.position = "none")

ggsave(temp_fig2, filename = './figures/name_origins_municipalities_br_denisty.png',
       width = 16, height = 16, units = 'cm', dpi=200)


# Separados
table(munis$name_origin)
temp_df <- subset(munis_df, name_origin != 'Indig.')

ggplot() +
  geom_sf(data=states, color='gray', fill='gray95', size=.2) +
  geom_pointdensity( data=temp_df, aes(x=x,y=y), size=.1, alpha=.5) +
  scale_color_viridis_c() +
  theme_minimal()

  
########### Map states ------------------------------------------------------------------

states <- merge(states, consolidated, by='abbrev_state')  
head(states)

fig_ind <- ggplot() +
            geom_sf(data=states, color='gray90', aes(fill=ind_prop)) +
            scale_fill_distiller(palette = 'Greens',direction = 1,
                                 name= 'indígena',labels = scales::percent_format(accuracy = 1)) +
            # scale_fill_viridis_c(name= 'indígena',labels = scales::percent_format(accuracy = 1)) +
            labs(title = "Proporção de municipios com nome de origem indígena",
                 caption = "Image by @UrbanDemog") +
            theme_void() +
            theme(plot.background = element_rect(fill = "white", color="white"))

fig_rel<- ggplot() +
          geom_sf(data=states, color='gray90', aes(fill=rel_prop)) +
          scale_fill_distiller(palette = 'Oranges',direction = 1,
                               name= 'religiosa',labels = scales::percent_format(accuracy = 1)) +
            # scale_fill_viridis_c(option = 'cividis', name= 'religioso',labels = scales::percent_format(accuracy = 1)) +
            labs(title = "Proporção de municipios com nome de origem religiosa",
                 caption = "Image by @UrbanDemog") +
            theme_void() +
            theme(plot.background = element_rect(fill = "white", color="white"))

fig_nat <- ggplot() +
            geom_sf(data=states, color='gray90', aes(fill=nat_prop )) +
            scale_fill_distiller(palette = 'Purples',direction = 1,
                                 name= 'natureza',labels = scales::percent_format(accuracy = 1)) +
            # scale_fill_viridis_c(option = 'cividis', name= 'religioso',labels = scales::percent_format(accuracy = 1)) +
            labs(title = "Proporção de municipios com nome relacionado a natureza",
                   caption = "Image by @UrbanDemog") +
            theme_void() +
            theme(plot.background = element_rect(fill = "white", color="white"))

ggsave(fig_rel, filename = './figures/states_rel.png', width = 16, height = 14, units = 'cm', dpi=200)
ggsave(fig_ind, filename = './figures/states_ind.png', width = 16, height = 14, units = 'cm', dpi=200)
ggsave(fig_nat, filename = './figures/states_nat.png', width = 16, height = 14, units = 'cm', dpi=200)



### bar plot states ---------------------


### random ---------------------
# rio
nrow( subset(munis, name_muni %like% 'Rio|Ribeir' ) ) / nrow(munis)

# cidades colônias
nrow( subset(munis, name_muni %like% 'Nova|Novo' ) ) / nrow(munis)

subset(munis, name_muni %like% 'Xer' )

