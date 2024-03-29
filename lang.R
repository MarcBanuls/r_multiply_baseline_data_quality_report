kLang <- list(
  EN = list(
    yes                  = "Yes",
    no                   = "No",
    head.survey          = "MULTIPLY Baseline HHS",
    head.report          = "Data Quality Report",
    head.github          = "Report GitHub Repo",
    progress.title       = "DATA COLLECTION PROGRESS ON THE FIELD",
    progress.records     = "The database contains %d records",
    progress.last        = "last record from %s",
    progress.partner     = "Data collection by",
    progress.subtitle1   = "General Progress",
    progress.women       = "Interviewed Children/Caretakers",
    progress.plot.title  = "Visited Households per cluster in %s",
    progress.plot.x      = "Cluster in %s",
    progress.plot.y      = "Number of households",
    progress.plot.s1     = "Interviewed",
    progress.plot.s2     = "Visited",
    progress.plot.hf     = "HF%s",
    progress.no.data     = "There's no data",
    profile.title        = "SURVEY PROFILE",
    profile.subtitle1    = "Profile of %s",
    profile.row2         = "HH selected visited",
    profile.row3         = "HH selected interviewed",
    profile.row4         = "HH with U5 children",
    profile.row5         = "Total of U5 children",
    profile.row6         = "HH with U2 children",
    profile.row7         = "Eligible children",
    profile.row8         = "Selected children whose caretaker is interviewed",
    profile.row9	 = "Selected children with performed RDT",
    profile.row10	 = "Selected children with RDT positive",
    profile.row11        = "Selected children whose caretaker is NOT interviewed",
    profile.row12        = "Caretakers that denied consent",
    profile.row13        = "Caretakers and/or children absent",
    profile.row14        = "Caretakers unable to respond",
    profile.row15        = "HH visited but HH head/adults does not accept to proceed with the interview",
    profile.row16        = "HH visited but contact with HH head/adult NOT made",
    profile.row17        = "HH empty or destroyed",
    profile.row18        = "HH heads/other adult not found",
    profile.check1       = "Visited HH must be equal to the sum of interviewed + NOT interviewed + contact with adult NOT made",
    profile.check2       = "HH with U5 children must be lower than the total of U5 children",
    profile.check3       = "HH with U5 children must be greater than HH with U2 children",
    profile.check4       = "Eligible children must be greater than the number of selected children interviewed + the number of selected children NON interviewed",
    profile.check5       = "Children interviewed must be equal or greater to number of RDT performed",
    profile.check6       = "Number of RDT must be greater or equal to the number of RDT positives",
    profile.check7       = "Children whose caretaker is NOT interviewed must be equal to the sum of denied + absent + unabled",
    profile.check8       = "HH visited but contact with adult not made must be equal to the number of HH empty or destroyed and the number of HH Head or other adult not found",
    profile.notes.title  = "Notes",
    profile.notes.desc   = "Colored cells are consistency errors. Hover over these cells to display a tooltip with the error message. Please, refer to the provided Data Queries Sheet. ",
    profile.note1        = "Eligible children: children that meets selection is between 10 and 23 months and is not participating has participated in ICARIA trial",
    dups.title           = "Duplicates",
    dups.subtitle1       = "Summary of duplicates in %s",
    dups.subtitle2       = "Duplicated Households",
    dups.subtitle3       = "Duplicated Records",
    dups.desc1           = "Households recorded / interviewed more than once by the same or different field workers. Or same household ID used more than once for different interviews.",
    dups.desc2           = "Records in which all variables are exactly the same. I.e. records sent more than once.",
    dups.no.hh           = "There are no duplicated households",
    dups.no.records      = "There are no duplicated records",
    dups.no.dups         = "There are no duplicates",
    dups.no.data         = "There is no data",
    dups.tab.header1     = "ID", 
    dups.tab.header2     = "District", 
    dups.tab.header3     = "Cluster", 
    dups.tab.header4     = "HH ID", 
    dups.tab.header5     = "Latitude", 
    dups.tab.header6     = "Longitude", 
    dups.tab.header7     = "Head Initials",
    dups.tab.header8     = "Consent", 
    dups.tab.header9     = "Int. ID",
    dups.tab.header10    = "Int. Date",
    dups.tab.header11    = "Int. ID RDT",
    dups.tab.header12    = "Int. Date RDT",
    dups.tab.header13    = "RDT Result",
    dups.tab.row2        = "Non interviewed HH",
    dups.tab.row3        = "Interviewed HH",
    dups.tab.row4        = "Duplicated records in NON interviewed HH",
    dups.tab.row5        = "Duplicated records in interviewed HH",
    dups.tab.row6        = "NON interviewed HH without duplicated records",
    dups.tab.row7        = "Interviewed HH without duplicated records",
    dups.tab.row8        = "Reused HH IDs",
    dups.tab.row9        = "Reused HH IDs in interviewed HH",
    indicators.title     = "MAIN INDICATORS",
    indicators.impt      = "Important",
    indicators.desc      = "These indicators are computed by using raw data. Therefore, data has not passed any verification and/or cleaning process. They should not be used for analysis purposes. They are presented to address any possible data issue.",
    indicators.subtitle1 = "SP Indicators",
    indicators.subtitle2 = "SP service provided in %s by CHW (c-IPTp)",
    indicators.subtitle3 = "by CHW (c-IPTp)",
    indicators.header1   = "Women who know",
    indicators.header2   = "Women who took",
    indicators.subtitle4 = "ANC Indicators",
    indicators.sp.row2   = "Women interviewed",
    indicators.sp.row3   = "Women that took SP",
    indicators.sp.row4   = "Women that took exactly 1 dose of SP",
    indicators.sp.row5   = "Women that took exactly 2 doses of SP",
    indicators.sp.row6   = "Women that took exactly 3 doses of SP",
    indicators.sp.row7   = "Women that took exactly 4 doses of SP",
    indicators.sp.row8   = "Women that took exactly 5 doses of SP",
    indicators.sp.row9   = "Women that took exactly 6 doses of SP",
    indicators.sp.row10  = "Women that took more than 6 doses of SP",
    indicators.sp.row11  = "Women that didn't take SP",
    indicators.sp.row12  = "Women that didn't know if they took SP",
    indicators.anc.row2  = "Women interviewed",
    indicators.anc.row3  = "Women that attended ANC clinic",
    indicators.anc.row4  = "Women that attended exactly once to ANC clinic",
    indicators.anc.row5  = "Women that attended exactly twice to ANC clinic",
    indicators.anc.row6  = "Women that attended exactly 3 times to ANC clinic",
    indicators.anc.row7  = "Women that attended exactly 4 times to ANC clinic",
    indicators.anc.row8  = "Women that attended exactly 5 times to ANC clinic",
    indicators.anc.row9  = "Women that attended exactly 6 times to ANC clinic",
    indicators.anc.row10 = "Women that attended more than 6 times to ANC clinic",
    indicators.anc.row11 = "Women that didn't attend to ANC clinic",
    rerecorded.sex1      = "F",
    rerecorded.sex2      = "M",
    rerecorded.empty     = "Empty HH",
    rerecorded.dup1      = "F",
    rerecorded.dup2      = "T",
    rerecorded.dups.h1   = "ID", 
    rerecorded.dups.h2   = "District", 
    rerecorded.dups.h3   = "C.", 
    rerecorded.dups.h4   = "HH ID", 
    rerecorded.dups.h5   = "Lat.", 
    rerecorded.dups.h6   = "Lng.", 
    rerecorded.dups.h7   = "H. Initials",
    rerecorded.dups.h8   = "Sex", 
    rerecorded.dups.h9   = "Available",
    rerecorded.dups.h10  = "Cons.",
    rerecorded.dups.h11  = "Child birth", 
    rerecorded.dups.h12  = "Int. ID",
    rerecorded.dups.h13  = "Int. Date",
    rerecorded.dups.h14  = "Int. ID RDT",
    rerecorded.dups.h15  = "Int. Date RDT",
    rerecorded.dups.h16  = "RDT Result",
    rerecorded.dups.h17  = "D.",
    not.asked            = "Not asked",
    rdt.positive         = "Positive",
    rdt.negative         = "Negative",
    rdt.indet            = "Indet"
  ),
  FR = list(
    yes                  = "Oui",
    no                   = "Non",
    head.survey          = "TIPTOP Endline HHS",
    head.report          = "Rapport sur la qualité des données",
    head.github          = "Dépôt GitHub du Rapport",
    progress.title       = "PROGRES DE LA COLLECTE DES DONNEES SUR LE TERRAIN",
    progress.records     = "La base de données contient %d enregistrements",
    progress.last        = "dernier enregistrement de %s",
    progress.partner     = "Collecte des données par",
    progress.subtitle1   = "Progrès général",
    progress.women       = "Femmes Interviewées",
    progress.plot.title  = "Ménages visités par cluster en %s",
    progress.plot.x      = "Cluster dans %s",
    progress.plot.y      = "Nombre de ménages",
    progress.plot.s1     = "Interviewées",
    progress.plot.s2     = "Visité",
    progress.plot.hf     = "HF%s",
    progress.no.data     = "Il n'y a pas de données",
    profile.title        = "PROFIL D'ENQUÊTE",
    profile.subtitle1    = "Profil de %s",
    profile.row2         = "HH sélectionné visité",
    profile.row3         = "HH sélectionné interviewé",
    profile.row4         = "Femmes en âge de procréer",
    profile.row5         = "Femmes NON éligibles",
    profile.row6         = "Femmes éligibles",
    profile.row7         = "Femmes éligibles sélectionnées",
    profile.row8         = "Femmes Interviewées",
    profile.row9         = "Femmes qui ont interrompu l'entretien",
    profile.row10        = "Femmes NON interviewées",
    profile.row11        = "Consentement/assentiment signé refusé",
    profile.row12        = "Absent",
    profile.row13        = "Impossible de répondre",
    profile.row14        = "Autre raison",
    profile.row15        = "HH sélectionné NON interviewé",
    profile.row16        = "Vide/détruit",
    profile.row17        = "Tête HH introuvable",
    profile.row18        = "Le chef de ménage/autre a refusé de consentir à l'entretien",
    profile.check1       = "HH NON interviewé doit être égal à la somme vide/détruit + refusé",
    profile.check2       = "Les femmes doivent être égales à la somme des éligibles + NON éligibles",
    profile.check3       = "Les femmes NON interviewées doivent être égales à la somme de refusé + absent + incapable",
    profile.check4       = "Les femmes sélectionnées doivent être égales à la somme des interviewées + interrompues + NON interviewées",
    profile.check5       = "Le HH visité doit être égal à la somme des interviewés + NON interviewés",
    profile.notes.title  = "Notes",
    profile.notes.desc   = "Les cellules colorées sont des erreurs de cohérence. Survolez ces cellules pour afficher une info-bulle avec le message d'erreur. Veuillez vous référer à la feuille de requêtes de données fournie.",
    profile.note1        = "Femme éligible : femme répondant aux critères de sélection 1 et aux critères de sélection 2",
    profile.note2        = "La disponibilité du chef de ménage n'est pas requise pour procéder à l'entretien tant que tout autre adulte y consent",
    dups.title           = "Doublons",
    dups.subtitle1       = "Résumé des doublons dans %s",
    dups.subtitle2       = "Ménages dupliqués",
    dups.subtitle3       = "Enregistrements dupliqués",
    dups.desc1           = "Ménages enregistrés / interrogés plus d'une fois par le même ou des agents de terrain différents. Ou même identifiant de ménage utilisé plus d'une fois pour différentes interviews.",
    dups.desc2           = "Enregistrements dans lesquels toutes les variables sont exactement les mêmes. C'est-à-dire les enregistrements envoyés plus d'une fois.",
    dups.no.hh           = "Il n'y a pas de ménages dupliqués",
    dups.no.records      = "Il n'y a aucun enregistrement en double",
    dups.no.dups         = "Il n'y a pas de doublons",
    dups.no.data         = "Il n'y a pas de données",
    dups.tab.header1     = "ID", 
    dups.tab.header2     = "District", 
    dups.tab.header3     = "Cluster", 
    dups.tab.header4     = "HH ID", 
    dups.tab.header5     = "Latitude", 
    dups.tab.header6     = "Longitude", 
    dups.tab.header7     = "Chef Initiales",
    dups.tab.header8     = "Consentement", 
    dups.tab.header9     = "Int. ID",
    dups.tab.header10    = "Int. Date",
    dups.tab.row2        = "HH non interviewé",
    dups.tab.row3        = "HH interviewé",
    dups.tab.row4        = "Enregistrements dupliqués dans les ménages NON interviewés",
    dups.tab.row5        = "Enregistrements dupliqués dans les ménages interviewés",
    dups.tab.row6        = "NON interviewé HH sans enregistrements en double",
    dups.tab.row7        = "HH interviewé sans enregistrements en double",
    dups.tab.row8        = "HH IDs réutilisés",
    dups.tab.row9        = "HH IDs réutilisésdans les HH interviewés",
    indicators.title     = "INDICATEURS PRINCIPAUX",
    indicators.impt      = "Important",
    indicators.desc      = "Ces indicateurs sont calculés en utilisant des données brutes. Par conséquent, les données n'ont passé aucun processus de vérification et/ou de nettoyage. Ils ne doivent pas être utilisés à des fins d'analyse. Ils sont présentés pour résoudre tout problème éventuel de données.",
    indicators.subtitle1 = "Indicateurs SP",
    indicators.subtitle2 = "Service SP fourni en %s par CHW (c-IPTp)",
    indicators.subtitle3 = "par ASC (c-IPTp)",
    indicators.header1   = "Femmes qui savent",
    indicators.header2   = "Femmes qui ont pris",
    indicators.subtitle4 = "Indicateurs ANC",
    indicators.sp.row2   = "Femmes interviewés",
    indicators.sp.row3   = "Femmes qui ont pris SP",
    indicators.sp.row4   = "Femmes qui ont pris exactement 1 dose de SP",
    indicators.sp.row5   = "Femmes qui ont pris exactement 2 doses de SP",
    indicators.sp.row6   = "Femmes qui ont pris exactement 3 doses de SP",
    indicators.sp.row7   = "Femmes qui ont pris exactement 4 doses de SP",
    indicators.sp.row8   = "Femmes qui ont pris exactement 5 doses de SP",
    indicators.sp.row9   = "Femmes qui ont pris exactement 6 doses de SP",
    indicators.sp.row10  = "Femmes ayant pris plus de 6 doses de SP",
    indicators.sp.row11  = "Femmes qui n'ont pas pris de SP",
    indicators.sp.row12  = "Femmes qui ne savaient pas si elles prenaient SP",
    indicators.anc.row2  = "Femmes interviewés",
    indicators.anc.row3  = "Femmes ayant fréquenté une CPN",
    indicators.anc.row4  = "Femmes qui se sont présentées exactement une fois à la CPN",
    indicators.anc.row5  = "Femmes qui se sont rendues exactement deux fois à la CPN",
    indicators.anc.row6  = "Femmes qui se sont rendues exactement 3 fois à la CPN",
    indicators.anc.row7  = "Femmes qui se sont rendues exactement 4 fois à la CPN",
    indicators.anc.row8  = "Femmes qui se sont présentées exactement 5 fois à la CPN",
    indicators.anc.row9  = "Femmes qui se sont rendues exactement 6 fois à la CPN",
    indicators.anc.row10 = "Femmes qui se sont rendues plus de 6 fois à la CPN",
    indicators.anc.row11 = "Femmes qui ne se sont pas rendues à la CPN",
    rerecorded.sex1      = "F",
    rerecorded.sex2      = "M",
    rerecorded.empty     = "Vide HH",
    rerecorded.dup1      = "F",
    rerecorded.dup2      = "T",
    rerecorded.dups.h1   = "ID", 
    rerecorded.dups.h2   = "District", 
    rerecorded.dups.h3   = "C.", 
    rerecorded.dups.h4   = "HH ID", 
    rerecorded.dups.h5   = "Lat.", 
    rerecorded.dups.h6   = "Lng.", 
    rerecorded.dups.h7   = "H. Initials",
    rerecorded.dups.h8   = "Sexe", 
    rerecorded.dups.h9   = "Disponible",
    rerecorded.dups.h10  = "Cons.",
    rerecorded.dups.h11  = "Terminer la preg.", 
    rerecorded.dups.h12  = "Âge",
    rerecorded.dups.h13  = "Int. ID",
    rerecorded.dups.h14  = "Int. Date",
    rerecorded.dups.h15  = "D.",
    not.asked            = "Pas demandé"
  ),
  PT = list(
    yes                  = "Sim",
    no                   = "Não",
    head.survey          = "TIPTOP Inquerito de Saída CPN",
    head.report          = "Relatório de Qualidade de Dados",
    head.github          = "Repositório GitHub",
    progress.title       = "PROGRESSO DE COLETA DE DADOS NO CAMPO",
    progress.records     = "A base de dados tem %d registros",
    progress.last        = "ultimo registro de %s",
    progress.partner     = "Coleta de dados por",
    progress.subtitle1   = "Progresso Geral",
    progress.women       = "Mulheres Entrevistadas",
    progress.plot.title  = "Mulheres abordadas por centro de saúde em %s",
    progress.plot.x      = "Centro de Saúde em %s",
    progress.plot.y      = "Número de mulheres",
    progress.plot.s1     = "Entrevistadas",
    progress.plot.s2     = "Abordadas",
    progress.plot.hf     = "CS%s",
    progress.no.data     = "Não há dados",
    profile.title        = "PERFIL DE PESQUISA",
    profile.subtitle1    = "Perfil de %s",
    profile.row2         = "HH selecionado visitado",
    profile.row3         = "HH selecionado entrevistado",
    profile.row4         = "Femmes en âge de procréer",
    profile.row5         = "Mulheres em idade fértil",
    profile.row6         = "Mulheres elegíveis",
    profile.row7         = "Mulheres elegíveis selecionadas",
    profile.row8         = "Mulheres Entrevistadas",
    profile.row9         = "Mulheres que interromperam a entrevista",
    profile.row10        = "Mulheres NÃO entrevistadas",
    profile.row11        = "Consentimento / assentimento assinado recusado",
    profile.row12        = "Ausente",
    profile.row13        = "Incapaz de responder",
    profile.row14        = "Outra razão",
    profile.row15        = "HH selecionado NÃO entrevistado",
    profile.row16        = "Vazio / destruído",
    profile.row17        = "Cabeça HH não encontrada",
    profile.row18        = "Chefe da família / outro recusou-se a consentir na entrevista",
    profile.check1       = "HH NÃO entrevistado deve ser igual à soma vazio / destruído + recusado",
    profile.check2       = "Mulheres devem ser iguais à soma de elegíveis + NÃO elegíveis",
    profile.check3       = "Mulheres NÃO entrevistadas deve ser igual à soma de recusados + ausentes + incapazes",
    profile.check4       = "As mulheres selecionadas devem ser iguais à soma de entrevistados + interrompidos + NÃO entrevistados",
    profile.check5       = "O HH visitado deve ser igual à soma dos entrevistados + NÃO entrevistados",
    profile.notes.title  = "Notas",
    profile.notes.desc   = "As células manchadas são erros de consistência. Passe o mouse sobre essas células para exibir uma dica de ferramenta com a mensagem de erro. Consulte a folha de solicitação de dados fornecida.",
    profile.note1        = "Mulher elegível: mulher que atende aos critérios de seleção 1 e aos critérios de seleção 2",
    profile.note2        = "A disponibilidade do chefe do agregado familiar não é exigida para prosseguir com a entrevista, desde que qualquer outro adulto dê o seu consentimento.",
    dups.title           = "Duplicados",
    dups.subtitle1       = "Resumo de duplicatas em %s",
    dups.subtitle2       = "Famílias duplicadas",
    dups.subtitle3       = "Registros duplicados",
    dups.desc1           = "Famílias registradas / entrevistadas mais de uma vez pelo mesmo ou por diferentes trabalhadores de campo. Ou mesmo o identificador doméstico usado mais de uma vez para diferentes entrevistas.",
    dups.desc2           = "Registros nos quais todas as variáveis são exatamente iguais. Ou seja, registros enviados mais de uma vez.",
    dups.no.hh           = "Não há residências duplicadas",
    dups.no.records      = "Não há registros duplicados",
    dups.no.dups         = "Não há duplicatas",
    dups.no.data         = "Não há dados",
    dups.tab.header1     = "ID", 
    dups.tab.header2     = "Distrito",
    dups.tab.header3     = "Cluster", 
    dups.tab.header4     = "HH ID", 
    dups.tab.header5     = "Latitude", 
    dups.tab.header6     = "Longitude", 
    dups.tab.header7     = "Iniciais Chefes",
    dups.tab.header8     = "Consentement", 
    dups.tab.header9     = "Int. ID",
    dups.tab.header10    = "Int. Encontro",
    dups.tab.row2        = "HH não entrevistado",
    dups.tab.row3        = "HH entrevistado",
    dups.tab.row4        = "Registros duplicados em famílias NÃO entrevistadas",
    dups.tab.row5        = "Registros duplicados em domicílios entrevistados",
    dups.tab.row6        = "NÃO entrevistou HH sem registros duplicados",
    dups.tab.row7        = "HH entrevistado sem registros duplicados",
    dups.tab.row8        = "HH IDs reutilizados",
    dups.tab.row9        = "HH IDs reutilizados no HH entrevistado",
    indicators.title     = "INDICADORES PRINCIPAIS",
    indicators.impt      = "Importante",
    indicators.desc      = "Esses indicadores são calculados com base em dados brutos. Portanto, os dados não passaram em nenhum processo de verificação e / ou limpeza. Eles não devem ser usados ​​para fins analíticos. Eles são apresentados para resolver qualquer possível problema de dados.",
    indicators.subtitle1 = "Indicadores SP",
    indicators.subtitle2 = "Serviço SP fornecido em %s pelo CHW (c-IPTp)",
    indicators.subtitle3 = "por ASC (c-IPTp)",
    indicators.header1   = "Mulheres que sabem",
    indicators.header2   = "Mulheres que tomaram",
    indicators.subtitle4 = "Indicadores ANC",
    indicators.sp.row2   = "Mulheres entrevistadas",
    indicators.sp.row3   = "Mulheres que fizeram SP",
    indicators.sp.row4   = "Mulheres que tomaram exatamente 1 dose de SP",
    indicators.sp.row5   = "Mulheres que tomaram exatamente 2 doses de SP",
    indicators.sp.row6   = "Mulheres que tomaram exatamente 3 doses de SP",
    indicators.sp.row7   = "Mulheres que tomaram exatamente 4 doses de SP",
    indicators.sp.row8   = "Mulheres que tomaram exatamente 5 doses de SP",
    indicators.sp.row9   = "Mulheres que tomaram exatamente 6 doses de SP",
    indicators.sp.row10  = "Mulheres que tomaram mais de 6 doses de SP",
    indicators.sp.row11  = "Mulheres que não fizeram SP",
    indicators.sp.row12  = "Mulheres que não sabiam se estavam tomando SP",
    indicators.anc.row2  = "Mulheres entrevistadas",
    indicators.anc.row3  = "Mulheres que frequentaram um ANC",
    indicators.anc.row4  = "Mulheres que se apresentaram exatamente uma vez ao ANC",
    indicators.anc.row5  = "Mulheres que estiveram no ANC exatamente duas vezes",
    indicators.anc.row6  = "Mulheres que estiveram no ANC exatamente 3 vezes",
    indicators.anc.row7  = "Mulheres que estiveram no ANC exatamente 4 vezes",
    indicators.anc.row8  = "Mulheres que se apresentaram ao ANC exatamente 5 vezes",
    indicators.anc.row9  = "Mulheres que foram ao ANC exatamente 6 vezes",
    indicators.anc.row10 = "Mulheres que já estiveram no ANC mais de 6 vezes",
    indicators.anc.row11 = "Mulheres que não frequentaram ANC",
    rerecorded.sex1      = "F",
    rerecorded.sex2      = "M",
    rerecorded.empty     = "HH Vazio",
    rerecorded.dup1      = "F",
    rerecorded.dup2      = "T",
    rerecorded.dups.h1   = "ID", 
    rerecorded.dups.h2   = "Distrito",
    rerecorded.dups.h3   = "C.", 
    rerecorded.dups.h4   = "HH ID", 
    rerecorded.dups.h5   = "Lat.", 
    rerecorded.dups.h6   = "Lng.", 
    rerecorded.dups.h7   = "H. Iniciais",
    rerecorded.dups.h8   = "Sexo",
    rerecorded.dups.h9   = "Disponível",
    rerecorded.dups.h10  = "Cons.",
    rerecorded.dups.h11  = "Fim do preg.",
    rerecorded.dups.h12  = "Era",
    rerecorded.dups.h13  = "Int. ID",
    rerecorded.dups.h14  = "Int. Encontro",
    rerecorded.dups.h15  = "D.",
    not.asked            = "Não perguntou"
  )
)