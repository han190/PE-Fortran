module euler_data_m

    implicit none

contains

    !> Data required for problem 0008.
    pure subroutine get_euler_data_0008(euler_data)
        character(len=:), allocatable, intent(out) :: euler_data(:)

        euler_data = &
            [character(len=50) :: &
             "73167176531330624919225119674426574742355349194934", &
             "96983520312774506326239578318016984801869478851843", &
             "85861560789112949495459501737958331952853208805511", &
             "12540698747158523863050715693290963295227443043557", &
             "66896648950445244523161731856403098711121722383113", &
             "62229893423380308135336276614282806444486645238749", &
             "30358907296290491560440772390713810515859307960866", &
             "70172427121883998797908792274921901699720888093776", &
             "65727333001053367881220235421809751254540594752243", &
             "52584907711670556013604839586446706324415722155397", &
             "53697817977846174064955149290862569321978468622482", &
             "83972241375657056057490261407972968652414535100474", &
             "82166370484403199890008895243450658541227588666881", &
             "16427171479924442928230863465674813919123162824586", &
             "17866458359124566529476545682848912883142607690042", &
             "24219022671055626321111109370544217506941658960408", &
             "07198403850962455444362981230987879927244284909188", &
             "84580156166097919133875499200524063689912560717606", &
             "05886116467109405077541002256983155200055935729725", &
             "71636269561882670428252483600823257530420752963450"]
    end subroutine get_euler_data_0008

    !> Data required for problem 0011.
    pure subroutine get_euler_data_0011(euler_data)
        character(len=:), allocatable, intent(out) :: euler_data(:)

        euler_data = &
            [character(len=59) :: &
             "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08", &
             "49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00", &
             "81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65", &
             "52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91", &
             "22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80", &
             "24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50", &
             "32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70", &
             "67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21", &
             "24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72", &
             "21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95", &
             "78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92", &
             "16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57", &
             "86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58", &
             "19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40", &
             "04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66", &
             "88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69", &
             "04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36", &
             "20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16", &
             "20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54", &
             "01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"]
    end subroutine get_euler_data_0011

    !> Data required for problem 0013.
    pure subroutine get_euler_data_0013(euler_data)
        character(len=:), allocatable, intent(out) :: euler_data(:)

        euler_data = &
            [character(len=50) :: &
             "37107287533902102798797998220837590246510135740250", &
             "46376937677490009712648124896970078050417018260538", &
             "74324986199524741059474233309513058123726617309629", &
             "91942213363574161572522430563301811072406154908250", &
             "23067588207539346171171980310421047513778063246676", &
             "89261670696623633820136378418383684178734361726757", &
             "28112879812849979408065481931592621691275889832738", &
             "44274228917432520321923589422876796487670272189318", &
             "47451445736001306439091167216856844588711603153276", &
             "70386486105843025439939619828917593665686757934951", &
             "62176457141856560629502157223196586755079324193331", &
             "64906352462741904929101432445813822663347944758178", &
             "92575867718337217661963751590579239728245598838407", &
             "58203565325359399008402633568948830189458628227828", &
             "80181199384826282014278194139940567587151170094390", &
             "35398664372827112653829987240784473053190104293586", &
             "86515506006295864861532075273371959191420517255829", &
             "71693888707715466499115593487603532921714970056938", &
             "54370070576826684624621495650076471787294438377604", &
             "53282654108756828443191190634694037855217779295145", &
             "36123272525000296071075082563815656710885258350721", &
             "45876576172410976447339110607218265236877223636045", &
             "17423706905851860660448207621209813287860733969412", &
             "81142660418086830619328460811191061556940512689692", &
             "51934325451728388641918047049293215058642563049483", &
             "62467221648435076201727918039944693004732956340691", &
             "15732444386908125794514089057706229429197107928209", &
             "55037687525678773091862540744969844508330393682126", &
             "18336384825330154686196124348767681297534375946515", &
             "80386287592878490201521685554828717201219257766954", &
             "78182833757993103614740356856449095527097864797581", &
             "16726320100436897842553539920931837441497806860984", &
             "48403098129077791799088218795327364475675590848030", &
             "87086987551392711854517078544161852424320693150332", &
             "59959406895756536782107074926966537676326235447210", &
             "69793950679652694742597709739166693763042633987085", &
             "41052684708299085211399427365734116182760315001271", &
             "65378607361501080857009149939512557028198746004375", &
             "35829035317434717326932123578154982629742552737307", &
             "94953759765105305946966067683156574377167401875275", &
             "88902802571733229619176668713819931811048770190271", &
             "25267680276078003013678680992525463401061632866526", &
             "36270218540497705585629946580636237993140746255962", &
             "24074486908231174977792365466257246923322810917141", &
             "91430288197103288597806669760892938638285025333403", &
             "34413065578016127815921815005561868836468420090470", &
             "23053081172816430487623791969842487255036638784583", &
             "11487696932154902810424020138335124462181441773470", &
             "63783299490636259666498587618221225225512486764533", &
             "67720186971698544312419572409913959008952310058822", &
             "95548255300263520781532296796249481641953868218774", &
             "76085327132285723110424803456124867697064507995236", &
             "37774242535411291684276865538926205024910326572967", &
             "23701913275725675285653248258265463092207058596522", &
             "29798860272258331913126375147341994889534765745501", &
             "18495701454879288984856827726077713721403798879715", &
             "38298203783031473527721580348144513491373226651381", &
             "34829543829199918180278916522431027392251122869539", &
             "40957953066405232632538044100059654939159879593635", &
             "29746152185502371307642255121183693803580388584903", &
             "41698116222072977186158236678424689157993532961922", &
             "62467957194401269043877107275048102390895523597457", &
             "23189706772547915061505504953922979530901129967519", &
             "86188088225875314529584099251203829009407770775672", &
             "11306739708304724483816533873502340845647058077308", &
             "82959174767140363198008187129011875491310547126581", &
             "97623331044818386269515456334926366572897563400500", &
             "42846280183517070527831839425882145521227251250327", &
             "55121603546981200581762165212827652751691296897789", &
             "32238195734329339946437501907836945765883352399886", &
             "75506164965184775180738168837861091527357929701337", &
             "62177842752192623401942399639168044983993173312731", &
             "32924185707147349566916674687634660915035914677504", &
             "99518671430235219628894890102423325116913619626622", &
             "73267460800591547471830798392868535206946944540724", &
             "76841822524674417161514036427982273348055556214818", &
             "97142617910342598647204516893989422179826088076852", &
             "87783646182799346313767754307809363333018982642090", &
             "10848802521674670883215120185883543223812876952786", &
             "71329612474782464538636993009049310363619763878039", &
             "62184073572399794223406235393808339651327408011116", &
             "66627891981488087797941876876144230030984490851411", &
             "60661826293682836764744779239180335110989069790714", &
             "85786944089552990653640447425576083659976645795096", &
             "66024396409905389607120198219976047599490197230297", &
             "64913982680032973156037120041377903785566085089252", &
             "16730939319872750275468906903707539413042652315011", &
             "94809377245048795150954100921645863754710598436791", &
             "78639167021187492431995700641917969777599028300699", &
             "15368713711936614952811305876380278410754449733078", &
             "40789923115535562561142322423255033685442488917353", &
             "44889911501440648020369068063960672322193204149535", &
             "41503128880339536053299340368006977710650566631954", &
             "81234880673210146739058568557934581403627822703280", &
             "82616570773948327592232845941706525094512325230608", &
             "22918802058777319719839450180888072429661980811197", &
             "77158542502016545090413245809786882778948721859617", &
             "72107838435069186155435662884062257473692284509516", &
             "20849603980134001723930671666823555245252804609722", &
             "53503534226472524250874054075591789781264330331690"]
    end subroutine get_euler_data_0013

    !> Data required for problem 0018.
    pure subroutine get_euler_data_0018(euler_data)
        character(len=:), allocatable, intent(out) :: euler_data(:)

        euler_data = &
            [character(len=50) :: &
             "75", &
             "95 64", &
             "17 47 82", &
             "18 35 87 10", &
             "20 04 82 47 65", &
             "19 01 23 75 03 34", &
             "88 02 77 73 07 63 67", &
             "99 65 04 28 06 16 70 92", &
             "41 41 26 56 83 40 80 70 33", &
             "41 48 72 33 47 32 37 16 94 29", &
             "53 71 44 65 25 43 91 52 97 51 14", &
             "70 11 33 28 77 73 17 78 39 68 17 57", &
             "91 71 52 38 17 14 91 43 58 50 27 29 48", &
             "63 66 04 68 89 53 67 30 73 16 69 87 40 31", &
             "04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"]
    end subroutine get_euler_data_0018

    !> Data required for problem 0022.
    pure subroutine get_euler_data_0022(euler_data)
        character(len=:), allocatable, intent(out) :: euler_data(:)

        euler_data = &
            [character(len=50) :: &
             "MARY", &
             "PATRICIA", &
             "LINDA", &
             "BARBARA", &
             "ELIZABETH", &
             "JENNIFER", &
             "MARIA", &
             "SUSAN", &
             "MARGARET", &
             "DOROTHY", &
             "LISA", &
             "NANCY", &
             "KAREN", &
             "BETTY", &
             "HELEN", &
             "SANDRA", &
             "DONNA", &
             "CAROL", &
             "RUTH", &
             "SHARON", &
             "MICHELLE", &
             "LAURA", &
             "SARAH", &
             "KIMBERLY", &
             "DEBORAH", &
             "JESSICA", &
             "SHIRLEY", &
             "CYNTHIA", &
             "ANGELA", &
             "MELISSA", &
             "BRENDA", &
             "AMY", &
             "ANNA", &
             "REBECCA", &
             "VIRGINIA", &
             "KATHLEEN", &
             "PAMELA", &
             "MARTHA", &
             "DEBRA", &
             "AMANDA", &
             "STEPHANIE", &
             "CAROLYN", &
             "CHRISTINE", &
             "MARIE", &
             "JANET", &
             "CATHERINE", &
             "FRANCES", &
             "ANN", &
             "JOYCE", &
             "DIANE", &
             "ALICE", &
             "JULIE", &
             "HEATHER", &
             "TERESA", &
             "DORIS", &
             "GLORIA", &
             "EVELYN", &
             "JEAN", &
             "CHERYL", &
             "MILDRED", &
             "KATHERINE", &
             "JOAN", &
             "ASHLEY", &
             "JUDITH", &
             "ROSE", &
             "JANICE", &
             "KELLY", &
             "NICOLE", &
             "JUDY", &
             "CHRISTINA", &
             "KATHY", &
             "THERESA", &
             "BEVERLY", &
             "DENISE", &
             "TAMMY", &
             "IRENE", &
             "JANE", &
             "LORI", &
             "RACHEL", &
             "MARILYN", &
             "ANDREA", &
             "KATHRYN", &
             "LOUISE", &
             "SARA", &
             "ANNE", &
             "JACQUELINE", &
             "WANDA", &
             "BONNIE", &
             "JULIA", &
             "RUBY", &
             "LOIS", &
             "TINA", &
             "PHYLLIS", &
             "NORMA", &
             "PAULA", &
             "DIANA", &
             "ANNIE", &
             "LILLIAN", &
             "EMILY", &
             "ROBIN", &
             "PEGGY", &
             "CRYSTAL", &
             "GLADYS", &
             "RITA", &
             "DAWN", &
             "CONNIE", &
             "FLORENCE", &
             "TRACY", &
             "EDNA", &
             "TIFFANY", &
             "CARMEN", &
             "ROSA", &
             "CINDY", &
             "GRACE", &
             "WENDY", &
             "VICTORIA", &
             "EDITH", &
             "KIM", &
             "SHERRY", &
             "SYLVIA", &
             "JOSEPHINE", &
             "THELMA", &
             "SHANNON", &
             "SHEILA", &
             "ETHEL", &
             "ELLEN", &
             "ELAINE", &
             "MARJORIE", &
             "CARRIE", &
             "CHARLOTTE", &
             "MONICA", &
             "ESTHER", &
             "PAULINE", &
             "EMMA", &
             "JUANITA", &
             "ANITA", &
             "RHONDA", &
             "HAZEL", &
             "AMBER", &
             "EVA", &
             "DEBBIE", &
             "APRIL", &
             "LESLIE", &
             "CLARA", &
             "LUCILLE", &
             "JAMIE", &
             "JOANNE", &
             "ELEANOR", &
             "VALERIE", &
             "DANIELLE", &
             "MEGAN", &
             "ALICIA", &
             "SUZANNE", &
             "MICHELE", &
             "GAIL", &
             "BERTHA", &
             "DARLENE", &
             "VERONICA", &
             "JILL", &
             "ERIN", &
             "GERALDINE", &
             "LAUREN", &
             "CATHY", &
             "JOANN", &
             "LORRAINE", &
             "LYNN", &
             "SALLY", &
             "REGINA", &
             "ERICA", &
             "BEATRICE", &
             "DOLORES", &
             "BERNICE", &
             "AUDREY", &
             "YVONNE", &
             "ANNETTE", &
             "JUNE", &
             "SAMANTHA", &
             "MARION", &
             "DANA", &
             "STACY", &
             "ANA", &
             "RENEE", &
             "IDA", &
             "VIVIAN", &
             "ROBERTA", &
             "HOLLY", &
             "BRITTANY", &
             "MELANIE", &
             "LORETTA", &
             "YOLANDA", &
             "JEANETTE", &
             "LAURIE", &
             "KATIE", &
             "KRISTEN", &
             "VANESSA", &
             "ALMA", &
             "SUE", &
             "ELSIE", &
             "BETH", &
             "JEANNE", &
             "VICKI", &
             "CARLA", &
             "TARA", &
             "ROSEMARY", &
             "EILEEN", &
             "TERRI", &
             "GERTRUDE", &
             "LUCY", &
             "TONYA", &
             "ELLA", &
             "STACEY", &
             "WILMA", &
             "GINA", &
             "KRISTIN", &
             "JESSIE", &
             "NATALIE", &
             "AGNES", &
             "VERA", &
             "WILLIE", &
             "CHARLENE", &
             "BESSIE", &
             "DELORES", &
             "MELINDA", &
             "PEARL", &
             "ARLENE", &
             "MAUREEN", &
             "COLLEEN", &
             "ALLISON", &
             "TAMARA", &
             "JOY", &
             "GEORGIA", &
             "CONSTANCE", &
             "LILLIE", &
             "CLAUDIA", &
             "JACKIE", &
             "MARCIA", &
             "TANYA", &
             "NELLIE", &
             "MINNIE", &
             "MARLENE", &
             "HEIDI", &
             "GLENDA", &
             "LYDIA", &
             "VIOLA", &
             "COURTNEY", &
             "MARIAN", &
             "STELLA", &
             "CAROLINE", &
             "DORA", &
             "JO", &
             "VICKIE", &
             "MATTIE", &
             "TERRY", &
             "MAXINE", &
             "IRMA", &
             "MABEL", &
             "MARSHA", &
             "MYRTLE", &
             "LENA", &
             "CHRISTY", &
             "DEANNA", &
             "PATSY", &
             "HILDA", &
             "GWENDOLYN", &
             "JENNIE", &
             "NORA", &
             "MARGIE", &
             "NINA", &
             "CASSANDRA", &
             "LEAH", &
             "PENNY", &
             "KAY", &
             "PRISCILLA", &
             "NAOMI", &
             "CAROLE", &
             "BRANDY", &
             "OLGA", &
             "BILLIE", &
             "DIANNE", &
             "TRACEY", &
             "LEONA", &
             "JENNY", &
             "FELICIA", &
             "SONIA", &
             "MIRIAM", &
             "VELMA", &
             "BECKY", &
             "BOBBIE", &
             "VIOLET", &
             "KRISTINA", &
             "TONI", &
             "MISTY", &
             "MAE", &
             "SHELLY", &
             "DAISY", &
             "RAMONA", &
             "SHERRI", &
             "ERIKA", &
             "KATRINA", &
             "CLAIRE", &
             "LINDSEY", &
             "LINDSAY", &
             "GENEVA", &
             "GUADALUPE", &
             "BELINDA", &
             "MARGARITA", &
             "SHERYL", &
             "CORA", &
             "FAYE", &
             "ADA", &
             "NATASHA", &
             "SABRINA", &
             "ISABEL", &
             "MARGUERITE", &
             "HATTIE", &
             "HARRIET", &
             "MOLLY", &
             "CECILIA", &
             "KRISTI", &
             "BRANDI", &
             "BLANCHE", &
             "SANDY", &
             "ROSIE", &
             "JOANNA", &
             "IRIS", &
             "EUNICE", &
             "ANGIE", &
             "INEZ", &
             "LYNDA", &
             "MADELINE", &
             "AMELIA", &
             "ALBERTA", &
             "GENEVIEVE", &
             "MONIQUE", &
             "JODI", &
             "JANIE", &
             "MAGGIE", &
             "KAYLA", &
             "SONYA", &
             "JAN", &
             "LEE", &
             "KRISTINE", &
             "CANDACE", &
             "FANNIE", &
             "MARYANN", &
             "OPAL", &
             "ALISON", &
             "YVETTE", &
             "MELODY", &
             "LUZ", &
             "SUSIE", &
             "OLIVIA", &
             "FLORA", &
             "SHELLEY", &
             "KRISTY", &
             "MAMIE", &
             "LULA", &
             "LOLA", &
             "VERNA", &
             "BEULAH", &
             "ANTOINETTE", &
             "CANDICE", &
             "JUANA", &
             "JEANNETTE", &
             "PAM", &
             "KELLI", &
             "HANNAH", &
             "WHITNEY", &
             "BRIDGET", &
             "KARLA", &
             "CELIA", &
             "LATOYA", &
             "PATTY", &
             "SHELIA", &
             "GAYLE", &
             "DELLA", &
             "VICKY", &
             "LYNNE", &
             "SHERI", &
             "MARIANNE", &
             "KARA", &
             "JACQUELYN", &
             "ERMA", &
             "BLANCA", &
             "MYRA", &
             "LETICIA", &
             "PAT", &
             "KRISTA", &
             "ROXANNE", &
             "ANGELICA", &
             "JOHNNIE", &
             "ROBYN", &
             "FRANCIS", &
             "ADRIENNE", &
             "ROSALIE", &
             "ALEXANDRA", &
             "BROOKE", &
             "BETHANY", &
             "SADIE", &
             "BERNADETTE", &
             "TRACI", &
             "JODY", &
             "KENDRA", &
             "JASMINE", &
             "NICHOLE", &
             "RACHAEL", &
             "CHELSEA", &
             "MABLE", &
             "ERNESTINE", &
             "MURIEL", &
             "MARCELLA", &
             "ELENA", &
             "KRYSTAL", &
             "ANGELINA", &
             "NADINE", &
             "KARI", &
             "ESTELLE", &
             "DIANNA", &
             "PAULETTE", &
             "LORA", &
             "MONA", &
             "DOREEN", &
             "ROSEMARIE", &
             "ANGEL", &
             "DESIREE", &
             "ANTONIA", &
             "HOPE", &
             "GINGER", &
             "JANIS", &
             "BETSY", &
             "CHRISTIE", &
             "FREDA", &
             "MERCEDES", &
             "MEREDITH", &
             "LYNETTE", &
             "TERI", &
             "CRISTINA", &
             "EULA", &
             "LEIGH", &
             "MEGHAN", &
             "SOPHIA", &
             "ELOISE", &
             "ROCHELLE", &
             "GRETCHEN", &
             "CECELIA", &
             "RAQUEL", &
             "HENRIETTA", &
             "ALYSSA", &
             "JANA", &
             "KELLEY", &
             "GWEN", &
             "KERRY", &
             "JENNA", &
             "TRICIA", &
             "LAVERNE", &
             "OLIVE", &
             "ALEXIS", &
             "TASHA", &
             "SILVIA", &
             "ELVIRA", &
             "CASEY", &
             "DELIA", &
             "SOPHIE", &
             "KATE", &
             "PATTI", &
             "LORENA", &
             "KELLIE", &
             "SONJA", &
             "LILA", &
             "LANA", &
             "DARLA", &
             "MAY", &
             "MINDY", &
             "ESSIE", &
             "MANDY", &
             "LORENE", &
             "ELSA", &
             "JOSEFINA", &
             "JEANNIE", &
             "MIRANDA", &
             "DIXIE", &
             "LUCIA", &
             "MARTA", &
             "FAITH", &
             "LELA", &
             "JOHANNA", &
             "SHARI", &
             "CAMILLE", &
             "TAMI", &
             "SHAWNA", &
             "ELISA", &
             "EBONY", &
             "MELBA", &
             "ORA", &
             "NETTIE", &
             "TABITHA", &
             "OLLIE", &
             "JAIME", &
             "WINIFRED", &
             "KRISTIE", &
             "MARINA", &
             "ALISHA", &
             "AIMEE", &
             "RENA", &
             "MYRNA", &
             "MARLA", &
             "TAMMIE", &
             "LATASHA", &
             "BONITA", &
             "PATRICE", &
             "RONDA", &
             "SHERRIE", &
             "ADDIE", &
             "FRANCINE", &
             "DELORIS", &
             "STACIE", &
             "ADRIANA", &
             "CHERI", &
             "SHELBY", &
             "ABIGAIL", &
             "CELESTE", &
             "JEWEL", &
             "CARA", &
             "ADELE", &
             "REBEKAH", &
             "LUCINDA", &
             "DORTHY", &
             "CHRIS", &
             "EFFIE", &
             "TRINA", &
             "REBA", &
             "SHAWN", &
             "SALLIE", &
             "AURORA", &
             "LENORA", &
             "ETTA", &
             "LOTTIE", &
             "KERRI", &
             "TRISHA", &
             "NIKKI", &
             "ESTELLA", &
             "FRANCISCA", &
             "JOSIE", &
             "TRACIE", &
             "MARISSA", &
             "KARIN", &
             "BRITTNEY", &
             "JANELLE", &
             "LOURDES", &
             "LAUREL", &
             "HELENE", &
             "FERN", &
             "ELVA", &
             "CORINNE", &
             "KELSEY", &
             "INA", &
             "BETTIE", &
             "ELISABETH", &
             "AIDA", &
             "CAITLIN", &
             "INGRID", &
             "IVA", &
             "EUGENIA", &
             "CHRISTA", &
             "GOLDIE", &
             "CASSIE", &
             "MAUDE", &
             "JENIFER", &
             "THERESE", &
             "FRANKIE", &
             "DENA", &
             "LORNA", &
             "JANETTE", &
             "LATONYA", &
             "CANDY", &
             "MORGAN", &
             "CONSUELO", &
             "TAMIKA", &
             "ROSETTA", &
             "DEBORA", &
             "CHERIE", &
             "POLLY", &
             "DINA", &
             "JEWELL", &
             "FAY", &
             "JILLIAN", &
             "DOROTHEA", &
             "NELL", &
             "TRUDY", &
             "ESPERANZA", &
             "PATRICA", &
             "KIMBERLEY", &
             "SHANNA", &
             "HELENA", &
             "CAROLINA", &
             "CLEO", &
             "STEFANIE", &
             "ROSARIO", &
             "OLA", &
             "JANINE", &
             "MOLLIE", &
             "LUPE", &
             "ALISA", &
             "LOU", &
             "MARIBEL", &
             "SUSANNE", &
             "BETTE", &
             "SUSANA", &
             "ELISE", &
             "CECILE", &
             "ISABELLE", &
             "LESLEY", &
             "JOCELYN", &
             "PAIGE", &
             "JONI", &
             "RACHELLE", &
             "LEOLA", &
             "DAPHNE", &
             "ALTA", &
             "ESTER", &
             "PETRA", &
             "GRACIELA", &
             "IMOGENE", &
             "JOLENE", &
             "KEISHA", &
             "LACEY", &
             "GLENNA", &
             "GABRIELA", &
             "KERI", &
             "URSULA", &
             "LIZZIE", &
             "KIRSTEN", &
             "SHANA", &
             "ADELINE", &
             "MAYRA", &
             "JAYNE", &
             "JACLYN", &
             "GRACIE", &
             "SONDRA", &
             "CARMELA", &
             "MARISA", &
             "ROSALIND", &
             "CHARITY", &
             "TONIA", &
             "BEATRIZ", &
             "MARISOL", &
             "CLARICE", &
             "JEANINE", &
             "SHEENA", &
             "ANGELINE", &
             "FRIEDA", &
             "LILY", &
             "ROBBIE", &
             "SHAUNA", &
             "MILLIE", &
             "CLAUDETTE", &
             "CATHLEEN", &
             "ANGELIA", &
             "GABRIELLE", &
             "AUTUMN", &
             "KATHARINE", &
             "SUMMER", &
             "JODIE", &
             "STACI", &
             "LEA", &
             "CHRISTI", &
             "JIMMIE", &
             "JUSTINE", &
             "ELMA", &
             "LUELLA", &
             "MARGRET", &
             "DOMINIQUE", &
             "SOCORRO", &
             "RENE", &
             "MARTINA", &
             "MARGO", &
             "MAVIS", &
             "CALLIE", &
             "BOBBI", &
             "MARITZA", &
             "LUCILE", &
             "LEANNE", &
             "JEANNINE", &
             "DEANA", &
             "AILEEN", &
             "LORIE", &
             "LADONNA", &
             "WILLA", &
             "MANUELA", &
             "GALE", &
             "SELMA", &
             "DOLLY", &
             "SYBIL", &
             "ABBY", &
             "LARA", &
             "DALE", &
             "IVY", &
             "DEE", &
             "WINNIE", &
             "MARCY", &
             "LUISA", &
             "JERI", &
             "MAGDALENA", &
             "OFELIA", &
             "MEAGAN", &
             "AUDRA", &
             "MATILDA", &
             "LEILA", &
             "CORNELIA", &
             "BIANCA", &
             "SIMONE", &
             "BETTYE", &
             "RANDI", &
             "VIRGIE", &
             "LATISHA", &
             "BARBRA", &
             "GEORGINA", &
             "ELIZA", &
             "LEANN", &
             "BRIDGETTE", &
             "RHODA", &
             "HALEY", &
             "ADELA", &
             "NOLA", &
             "BERNADINE", &
             "FLOSSIE", &
             "ILA", &
             "GRETA", &
             "RUTHIE", &
             "NELDA", &
             "MINERVA", &
             "LILLY", &
             "TERRIE", &
             "LETHA", &
             "HILARY", &
             "ESTELA", &
             "VALARIE", &
             "BRIANNA", &
             "ROSALYN", &
             "EARLINE", &
             "CATALINA", &
             "AVA", &
             "MIA", &
             "CLARISSA", &
             "LIDIA", &
             "CORRINE", &
             "ALEXANDRIA", &
             "CONCEPCION", &
             "TIA", &
             "SHARRON", &
             "RAE", &
             "DONA", &
             "ERICKA", &
             "JAMI", &
             "ELNORA", &
             "CHANDRA", &
             "LENORE", &
             "NEVA", &
             "MARYLOU", &
             "MELISA", &
             "TABATHA", &
             "SERENA", &
             "AVIS", &
             "ALLIE", &
             "SOFIA", &
             "JEANIE", &
             "ODESSA", &
             "NANNIE", &
             "HARRIETT", &
             "LORAINE", &
             "PENELOPE", &
             "MILAGROS", &
             "EMILIA", &
             "BENITA", &
             "ALLYSON", &
             "ASHLEE", &
             "TANIA", &
             "TOMMIE", &
             "ESMERALDA", &
             "KARINA", &
             "EVE", &
             "PEARLIE", &
             "ZELMA", &
             "MALINDA", &
             "NOREEN", &
             "TAMEKA", &
             "SAUNDRA", &
             "HILLARY", &
             "AMIE", &
             "ALTHEA", &
             "ROSALINDA", &
             "JORDAN", &
             "LILIA", &
             "ALANA", &
             "GAY", &
             "CLARE", &
             "ALEJANDRA", &
             "ELINOR", &
             "MICHAEL", &
             "LORRIE", &
             "JERRI", &
             "DARCY", &
             "EARNESTINE", &
             "CARMELLA", &
             "TAYLOR", &
             "NOEMI", &
             "MARCIE", &
             "LIZA", &
             "ANNABELLE", &
             "LOUISA", &
             "EARLENE", &
             "MALLORY", &
             "CARLENE", &
             "NITA", &
             "SELENA", &
             "TANISHA", &
             "KATY", &
             "JULIANNE", &
             "JOHN", &
             "LAKISHA", &
             "EDWINA", &
             "MARICELA", &
             "MARGERY", &
             "KENYA", &
             "DOLLIE", &
             "ROXIE", &
             "ROSLYN", &
             "KATHRINE", &
             "NANETTE", &
             "CHARMAINE", &
             "LAVONNE", &
             "ILENE", &
             "KRIS", &
             "TAMMI", &
             "SUZETTE", &
             "CORINE", &
             "KAYE", &
             "JERRY", &
             "MERLE", &
             "CHRYSTAL", &
             "LINA", &
             "DEANNE", &
             "LILIAN", &
             "JULIANA", &
             "ALINE", &
             "LUANN", &
             "KASEY", &
             "MARYANNE", &
             "EVANGELINE", &
             "COLETTE", &
             "MELVA", &
             "LAWANDA", &
             "YESENIA", &
             "NADIA", &
             "MADGE", &
             "KATHIE", &
             "EDDIE", &
             "OPHELIA", &
             "VALERIA", &
             "NONA", &
             "MITZI", &
             "MARI", &
             "GEORGETTE", &
             "CLAUDINE", &
             "FRAN", &
             "ALISSA", &
             "ROSEANN", &
             "LAKEISHA", &
             "SUSANNA", &
             "REVA", &
             "DEIDRE", &
             "CHASITY", &
             "SHEREE", &
             "CARLY", &
             "JAMES", &
             "ELVIA", &
             "ALYCE", &
             "DEIRDRE", &
             "GENA", &
             "BRIANA", &
             "ARACELI", &
             "KATELYN", &
             "ROSANNE", &
             "WENDI", &
             "TESSA", &
             "BERTA", &
             "MARVA", &
             "IMELDA", &
             "MARIETTA", &
             "MARCI", &
             "LEONOR", &
             "ARLINE", &
             "SASHA", &
             "MADELYN", &
             "JANNA", &
             "JULIETTE", &
             "DEENA", &
             "AURELIA", &
             "JOSEFA", &
             "AUGUSTA", &
             "LILIANA", &
             "YOUNG", &
             "CHRISTIAN", &
             "LESSIE", &
             "AMALIA", &
             "SAVANNAH", &
             "ANASTASIA", &
             "VILMA", &
             "NATALIA", &
             "ROSELLA", &
             "LYNNETTE", &
             "CORINA", &
             "ALFREDA", &
             "LEANNA", &
             "CAREY", &
             "AMPARO", &
             "COLEEN", &
             "TAMRA", &
             "AISHA", &
             "WILDA", &
             "KARYN", &
             "CHERRY", &
             "QUEEN", &
             "MAURA", &
             "MAI", &
             "EVANGELINA", &
             "ROSANNA", &
             "HALLIE", &
             "ERNA", &
             "ENID", &
             "MARIANA", &
             "LACY", &
             "JULIET", &
             "JACKLYN", &
             "FREIDA", &
             "MADELEINE", &
             "MARA", &
             "HESTER", &
             "CATHRYN", &
             "LELIA", &
             "CASANDRA", &
             "BRIDGETT", &
             "ANGELITA", &
             "JANNIE", &
             "DIONNE", &
             "ANNMARIE", &
             "KATINA", &
             "BERYL", &
             "PHOEBE", &
             "MILLICENT", &
             "KATHERYN", &
             "DIANN", &
             "CARISSA", &
             "MARYELLEN", &
             "LIZ", &
             "LAURI", &
             "HELGA", &
             "GILDA", &
             "ADRIAN", &
             "RHEA", &
             "MARQUITA", &
             "HOLLIE", &
             "TISHA", &
             "TAMERA", &
             "ANGELIQUE", &
             "FRANCESCA", &
             "BRITNEY", &
             "KAITLIN", &
             "LOLITA", &
             "FLORINE", &
             "ROWENA", &
             "REYNA", &
             "TWILA", &
             "FANNY", &
             "JANELL", &
             "INES", &
             "CONCETTA", &
             "BERTIE", &
             "ALBA", &
             "BRIGITTE", &
             "ALYSON", &
             "VONDA", &
             "PANSY", &
             "ELBA", &
             "NOELLE", &
             "LETITIA", &
             "KITTY", &
             "DEANN", &
             "BRANDIE", &
             "LOUELLA", &
             "LETA", &
             "FELECIA", &
             "SHARLENE", &
             "LESA", &
             "BEVERLEY", &
             "ROBERT", &
             "ISABELLA", &
             "HERMINIA", &
             "TERRA", &
             "CELINA", &
             "TORI", &
             "OCTAVIA", &
             "JADE", &
             "DENICE", &
             "GERMAINE", &
             "SIERRA", &
             "MICHELL", &
             "CORTNEY", &
             "NELLY", &
             "DORETHA", &
             "SYDNEY", &
             "DEIDRA", &
             "MONIKA", &
             "LASHONDA", &
             "JUDI", &
             "CHELSEY", &
             "ANTIONETTE", &
             "MARGOT", &
             "BOBBY", &
             "ADELAIDE", &
             "NAN", &
             "LEEANN", &
             "ELISHA", &
             "DESSIE", &
             "LIBBY", &
             "KATHI", &
             "GAYLA", &
             "LATANYA", &
             "MINA", &
             "MELLISA", &
             "KIMBERLEE", &
             "JASMIN", &
             "RENAE", &
             "ZELDA", &
             "ELDA", &
             "MA", &
             "JUSTINA", &
             "GUSSIE", &
             "EMILIE", &
             "CAMILLA", &
             "ABBIE", &
             "ROCIO", &
             "KAITLYN", &
             "JESSE", &
             "EDYTHE", &
             "ASHLEIGH", &
             "SELINA", &
             "LAKESHA", &
             "GERI", &
             "ALLENE", &
             "PAMALA", &
             "MICHAELA", &
             "DAYNA", &
             "CARYN", &
             "ROSALIA", &
             "SUN", &
             "JACQULINE", &
             "REBECA", &
             "MARYBETH", &
             "KRYSTLE", &
             "IOLA", &
             "DOTTIE", &
             "BENNIE", &
             "BELLE", &
             "AUBREY", &
             "GRISELDA", &
             "ERNESTINA", &
             "ELIDA", &
             "ADRIANNE", &
             "DEMETRIA", &
             "DELMA", &
             "CHONG", &
             "JAQUELINE", &
             "DESTINY", &
             "ARLEEN", &
             "VIRGINA", &
             "RETHA", &
             "FATIMA", &
             "TILLIE", &
             "ELEANORE", &
             "CARI", &
             "TREVA", &
             "BIRDIE", &
             "WILHELMINA", &
             "ROSALEE", &
             "MAURINE", &
             "LATRICE", &
             "YONG", &
             "JENA", &
             "TARYN", &
             "ELIA", &
             "DEBBY", &
             "MAUDIE", &
             "JEANNA", &
             "DELILAH", &
             "CATRINA", &
             "SHONDA", &
             "HORTENCIA", &
             "THEODORA", &
             "TERESITA", &
             "ROBBIN", &
             "DANETTE", &
             "MARYJANE", &
             "FREDDIE", &
             "DELPHINE", &
             "BRIANNE", &
             "NILDA", &
             "DANNA", &
             "CINDI", &
             "BESS", &
             "IONA", &
             "HANNA", &
             "ARIEL", &
             "WINONA", &
             "VIDA", &
             "ROSITA", &
             "MARIANNA", &
             "WILLIAM", &
             "RACHEAL", &
             "GUILLERMINA", &
             "ELOISA", &
             "CELESTINE", &
             "CAREN", &
             "MALISSA", &
             "LONA", &
             "CHANTEL", &
             "SHELLIE", &
             "MARISELA", &
             "LEORA", &
             "AGATHA", &
             "SOLEDAD", &
             "MIGDALIA", &
             "IVETTE", &
             "CHRISTEN", &
             "ATHENA", &
             "JANEL", &
             "CHLOE", &
             "VEDA", &
             "PATTIE", &
             "TESSIE", &
             "TERA", &
             "MARILYNN", &
             "LUCRETIA", &
             "KARRIE", &
             "DINAH", &
             "DANIELA", &
             "ALECIA", &
             "ADELINA", &
             "VERNICE", &
             "SHIELA", &
             "PORTIA", &
             "MERRY", &
             "LASHAWN", &
             "DEVON", &
             "DARA", &
             "TAWANA", &
             "OMA", &
             "VERDA", &
             "CHRISTIN", &
             "ALENE", &
             "ZELLA", &
             "SANDI", &
             "RAFAELA", &
             "MAYA", &
             "KIRA", &
             "CANDIDA", &
             "ALVINA", &
             "SUZAN", &
             "SHAYLA", &
             "LYN", &
             "LETTIE", &
             "ALVA", &
             "SAMATHA", &
             "ORALIA", &
             "MATILDE", &
             "MADONNA", &
             "LARISSA", &
             "VESTA", &
             "RENITA", &
             "INDIA", &
             "DELOIS", &
             "SHANDA", &
             "PHILLIS", &
             "LORRI", &
             "ERLINDA", &
             "CRUZ", &
             "CATHRINE", &
             "BARB", &
             "ZOE", &
             "ISABELL", &
             "IONE", &
             "GISELA", &
             "CHARLIE", &
             "VALENCIA", &
             "ROXANNA", &
             "MAYME", &
             "KISHA", &
             "ELLIE", &
             "MELLISSA", &
             "DORRIS", &
             "DALIA", &
             "BELLA", &
             "ANNETTA", &
             "ZOILA", &
             "RETA", &
             "REINA", &
             "LAURETTA", &
             "KYLIE", &
             "CHRISTAL", &
             "PILAR", &
             "CHARLA", &
             "ELISSA", &
             "TIFFANI", &
             "TANA", &
             "PAULINA", &
             "LEOTA", &
             "BREANNA", &
             "JAYME", &
             "CARMEL", &
             "VERNELL", &
             "TOMASA", &
             "MANDI", &
             "DOMINGA", &
             "SANTA", &
             "MELODIE", &
             "LURA", &
             "ALEXA", &
             "TAMELA", &
             "RYAN", &
             "MIRNA", &
             "KERRIE", &
             "VENUS", &
             "NOEL", &
             "FELICITA", &
             "CRISTY", &
             "CARMELITA", &
             "BERNIECE", &
             "ANNEMARIE", &
             "TIARA", &
             "ROSEANNE", &
             "MISSY", &
             "CORI", &
             "ROXANA", &
             "PRICILLA", &
             "KRISTAL", &
             "JUNG", &
             "ELYSE", &
             "HAYDEE", &
             "ALETHA", &
             "BETTINA", &
             "MARGE", &
             "GILLIAN", &
             "FILOMENA", &
             "CHARLES", &
             "ZENAIDA", &
             "HARRIETTE", &
             "CARIDAD", &
             "VADA", &
             "UNA", &
             "ARETHA", &
             "PEARLINE", &
             "MARJORY", &
             "MARCELA", &
             "FLOR", &
             "EVETTE", &
             "ELOUISE", &
             "ALINA", &
             "TRINIDAD", &
             "DAVID", &
             "DAMARIS", &
             "CATHARINE", &
             "CARROLL", &
             "BELVA", &
             "NAKIA", &
             "MARLENA", &
             "LUANNE", &
             "LORINE", &
             "KARON", &
             "DORENE", &
             "DANITA", &
             "BRENNA", &
             "TATIANA", &
             "SAMMIE", &
             "LOUANN", &
             "LOREN", &
             "JULIANNA", &
             "ANDRIA", &
             "PHILOMENA", &
             "LUCILA", &
             "LEONORA", &
             "DOVIE", &
             "ROMONA", &
             "MIMI", &
             "JACQUELIN", &
             "GAYE", &
             "TONJA", &
             "MISTI", &
             "JOE", &
             "GENE", &
             "CHASTITY", &
             "STACIA", &
             "ROXANN", &
             "MICAELA", &
             "NIKITA", &
             "MEI", &
             "VELDA", &
             "MARLYS", &
             "JOHNNA", &
             "AURA", &
             "LAVERN", &
             "IVONNE", &
             "HAYLEY", &
             "NICKI", &
             "MAJORIE", &
             "HERLINDA", &
             "GEORGE", &
             "ALPHA", &
             "YADIRA", &
             "PERLA", &
             "GREGORIA", &
             "DANIEL", &
             "ANTONETTE", &
             "SHELLI", &
             "MOZELLE", &
             "MARIAH", &
             "JOELLE", &
             "CORDELIA", &
             "JOSETTE", &
             "CHIQUITA", &
             "TRISTA", &
             "LOUIS", &
             "LAQUITA", &
             "GEORGIANA", &
             "CANDI", &
             "SHANON", &
             "LONNIE", &
             "HILDEGARD", &
             "CECIL", &
             "VALENTINA", &
             "STEPHANY", &
             "MAGDA", &
             "KAROL", &
             "GERRY", &
             "GABRIELLA", &
             "TIANA", &
             "ROMA", &
             "RICHELLE", &
             "RAY", &
             "PRINCESS", &
             "OLETA", &
             "JACQUE", &
             "IDELLA", &
             "ALAINA", &
             "SUZANNA", &
             "JOVITA", &
             "BLAIR", &
             "TOSHA", &
             "RAVEN", &
             "NEREIDA", &
             "MARLYN", &
             "KYLA", &
             "JOSEPH", &
             "DELFINA", &
             "TENA", &
             "STEPHENIE", &
             "SABINA", &
             "NATHALIE", &
             "MARCELLE", &
             "GERTIE", &
             "DARLEEN", &
             "THEA", &
             "SHARONDA", &
             "SHANTEL", &
             "BELEN", &
             "VENESSA", &
             "ROSALINA", &
             "ONA", &
             "GENOVEVA", &
             "COREY", &
             "CLEMENTINE", &
             "ROSALBA", &
             "RENATE", &
             "RENATA", &
             "MI", &
             "IVORY", &
             "GEORGIANNA", &
             "FLOY", &
             "DORCAS", &
             "ARIANA", &
             "TYRA", &
             "THEDA", &
             "MARIAM", &
             "JULI", &
             "JESICA", &
             "DONNIE", &
             "VIKKI", &
             "VERLA", &
             "ROSELYN", &
             "MELVINA", &
             "JANNETTE", &
             "GINNY", &
             "DEBRAH", &
             "CORRIE", &
             "ASIA", &
             "VIOLETA", &
             "MYRTIS", &
             "LATRICIA", &
             "COLLETTE", &
             "CHARLEEN", &
             "ANISSA", &
             "VIVIANA", &
             "TWYLA", &
             "PRECIOUS", &
             "NEDRA", &
             "LATONIA", &
             "LAN", &
             "HELLEN", &
             "FABIOLA", &
             "ANNAMARIE", &
             "ADELL", &
             "SHARYN", &
             "CHANTAL", &
             "NIKI", &
             "MAUD", &
             "LIZETTE", &
             "LINDY", &
             "KIA", &
             "KESHA", &
             "JEANA", &
             "DANELLE", &
             "CHARLINE", &
             "CHANEL", &
             "CARROL", &
             "VALORIE", &
             "LIA", &
             "DORTHA", &
             "CRISTAL", &
             "SUNNY", &
             "LEONE", &
             "LEILANI", &
             "GERRI", &
             "DEBI", &
             "ANDRA", &
             "KESHIA", &
             "IMA", &
             "EULALIA", &
             "EASTER", &
             "DULCE", &
             "NATIVIDAD", &
             "LINNIE", &
             "KAMI", &
             "GEORGIE", &
             "CATINA", &
             "BROOK", &
             "ALDA", &
             "WINNIFRED", &
             "SHARLA", &
             "RUTHANN", &
             "MEAGHAN", &
             "MAGDALENE", &
             "LISSETTE", &
             "ADELAIDA", &
             "VENITA", &
             "TRENA", &
             "SHIRLENE", &
             "SHAMEKA", &
             "ELIZEBETH", &
             "DIAN", &
             "SHANTA", &
             "MICKEY", &
             "LATOSHA", &
             "CARLOTTA", &
             "WINDY", &
             "SOON", &
             "ROSINA", &
             "MARIANN", &
             "LEISA", &
             "JONNIE", &
             "DAWNA", &
             "CATHIE", &
             "BILLY", &
             "ASTRID", &
             "SIDNEY", &
             "LAUREEN", &
             "JANEEN", &
             "HOLLI", &
             "FAWN", &
             "VICKEY", &
             "TERESSA", &
             "SHANTE", &
             "RUBYE", &
             "MARCELINA", &
             "CHANDA", &
             "CARY", &
             "TERESE", &
             "SCARLETT", &
             "MARTY", &
             "MARNIE", &
             "LULU", &
             "LISETTE", &
             "JENIFFER", &
             "ELENOR", &
             "DORINDA", &
             "DONITA", &
             "CARMAN", &
             "BERNITA", &
             "ALTAGRACIA", &
             "ALETA", &
             "ADRIANNA", &
             "ZORAIDA", &
             "RONNIE", &
             "NICOLA", &
             "LYNDSEY", &
             "KENDALL", &
             "JANINA", &
             "CHRISSY", &
             "AMI", &
             "STARLA", &
             "PHYLIS", &
             "PHUONG", &
             "KYRA", &
             "CHARISSE", &
             "BLANCH", &
             "SANJUANITA", &
             "RONA", &
             "NANCI", &
             "MARILEE", &
             "MARANDA", &
             "CORY", &
             "BRIGETTE", &
             "SANJUANA", &
             "MARITA", &
             "KASSANDRA", &
             "JOYCELYN", &
             "IRA", &
             "FELIPA", &
             "CHELSIE", &
             "BONNY", &
             "MIREYA", &
             "LORENZA", &
             "KYONG", &
             "ILEANA", &
             "CANDELARIA", &
             "TONY", &
             "TOBY", &
             "SHERIE", &
             "OK", &
             "MARK", &
             "LUCIE", &
             "LEATRICE", &
             "LAKESHIA", &
             "GERDA", &
             "EDIE", &
             "BAMBI", &
             "MARYLIN", &
             "LAVON", &
             "HORTENSE", &
             "GARNET", &
             "EVIE", &
             "TRESSA", &
             "SHAYNA", &
             "LAVINA", &
             "KYUNG", &
             "JEANETTA", &
             "SHERRILL", &
             "SHARA", &
             "PHYLISS", &
             "MITTIE", &
             "ANABEL", &
             "ALESIA", &
             "THUY", &
             "TAWANDA", &
             "RICHARD", &
             "JOANIE", &
             "TIFFANIE", &
             "LASHANDA", &
             "KARISSA", &
             "ENRIQUETA", &
             "DARIA", &
             "DANIELLA", &
             "CORINNA", &
             "ALANNA", &
             "ABBEY", &
             "ROXANE", &
             "ROSEANNA", &
             "MAGNOLIA", &
             "LIDA", &
             "KYLE", &
             "JOELLEN", &
             "ERA", &
             "CORAL", &
             "CARLEEN", &
             "TRESA", &
             "PEGGIE", &
             "NOVELLA", &
             "NILA", &
             "MAYBELLE", &
             "JENELLE", &
             "CARINA", &
             "NOVA", &
             "MELINA", &
             "MARQUERITE", &
             "MARGARETTE", &
             "JOSEPHINA", &
             "EVONNE", &
             "DEVIN", &
             "CINTHIA", &
             "ALBINA", &
             "TOYA", &
             "TAWNYA", &
             "SHERITA", &
             "SANTOS", &
             "MYRIAM", &
             "LIZABETH", &
             "LISE", &
             "KEELY", &
             "JENNI", &
             "GISELLE", &
             "CHERYLE", &
             "ARDITH", &
             "ARDIS", &
             "ALESHA", &
             "ADRIANE", &
             "SHAINA", &
             "LINNEA", &
             "KAROLYN", &
             "HONG", &
             "FLORIDA", &
             "FELISHA", &
             "DORI", &
             "DARCI", &
             "ARTIE", &
             "ARMIDA", &
             "ZOLA", &
             "XIOMARA", &
             "VERGIE", &
             "SHAMIKA", &
             "NENA", &
             "NANNETTE", &
             "MAXIE", &
             "LOVIE", &
             "JEANE", &
             "JAIMIE", &
             "INGE", &
             "FARRAH", &
             "ELAINA", &
             "CAITLYN", &
             "STARR", &
             "FELICITAS", &
             "CHERLY", &
             "CARYL", &
             "YOLONDA", &
             "YASMIN", &
             "TEENA", &
             "PRUDENCE", &
             "PENNIE", &
             "NYDIA", &
             "MACKENZIE", &
             "ORPHA", &
             "MARVEL", &
             "LIZBETH", &
             "LAURETTE", &
             "JERRIE", &
             "HERMELINDA", &
             "CAROLEE", &
             "TIERRA", &
             "MIRIAN", &
             "META", &
             "MELONY", &
             "KORI", &
             "JENNETTE", &
             "JAMILA", &
             "ENA", &
             "ANH", &
             "YOSHIKO", &
             "SUSANNAH", &
             "SALINA", &
             "RHIANNON", &
             "JOLEEN", &
             "CRISTINE", &
             "ASHTON", &
             "ARACELY", &
             "TOMEKA", &
             "SHALONDA", &
             "MARTI", &
             "LACIE", &
             "KALA", &
             "JADA", &
             "ILSE", &
             "HAILEY", &
             "BRITTANI", &
             "ZONA", &
             "SYBLE", &
             "SHERRYL", &
             "RANDY", &
             "NIDIA", &
             "MARLO", &
             "KANDICE", &
             "KANDI", &
             "DEB", &
             "DEAN", &
             "AMERICA", &
             "ALYCIA", &
             "TOMMY", &
             "RONNA", &
             "NORENE", &
             "MERCY", &
             "JOSE", &
             "INGEBORG", &
             "GIOVANNA", &
             "GEMMA", &
             "CHRISTEL", &
             "AUDRY", &
             "ZORA", &
             "VITA", &
             "VAN", &
             "TRISH", &
             "STEPHAINE", &
             "SHIRLEE", &
             "SHANIKA", &
             "MELONIE", &
             "MAZIE", &
             "JAZMIN", &
             "INGA", &
             "HOA", &
             "HETTIE", &
             "GERALYN", &
             "FONDA", &
             "ESTRELLA", &
             "ADELLA", &
             "SU", &
             "SARITA", &
             "RINA", &
             "MILISSA", &
             "MARIBETH", &
             "GOLDA", &
             "EVON", &
             "ETHELYN", &
             "ENEDINA", &
             "CHERISE", &
             "CHANA", &
             "VELVA", &
             "TAWANNA", &
             "SADE", &
             "MIRTA", &
             "LI", &
             "KARIE", &
             "JACINTA", &
             "ELNA", &
             "DAVINA", &
             "CIERRA", &
             "ASHLIE", &
             "ALBERTHA", &
             "TANESHA", &
             "STEPHANI", &
             "NELLE", &
             "MINDI", &
             "LU", &
             "LORINDA", &
             "LARUE", &
             "FLORENE", &
             "DEMETRA", &
             "DEDRA", &
             "CIARA", &
             "CHANTELLE", &
             "ASHLY", &
             "SUZY", &
             "ROSALVA", &
             "NOELIA", &
             "LYDA", &
             "LEATHA", &
             "KRYSTYNA", &
             "KRISTAN", &
             "KARRI", &
             "DARLINE", &
             "DARCIE", &
             "CINDA", &
             "CHEYENNE", &
             "CHERRIE", &
             "AWILDA", &
             "ALMEDA", &
             "ROLANDA", &
             "LANETTE", &
             "JERILYN", &
             "GISELE", &
             "EVALYN", &
             "CYNDI", &
             "CLETA", &
             "CARIN", &
             "ZINA", &
             "ZENA", &
             "VELIA", &
             "TANIKA", &
             "PAUL", &
             "CHARISSA", &
             "THOMAS", &
             "TALIA", &
             "MARGARETE", &
             "LAVONDA", &
             "KAYLEE", &
             "KATHLENE", &
             "JONNA", &
             "IRENA", &
             "ILONA", &
             "IDALIA", &
             "CANDIS", &
             "CANDANCE", &
             "BRANDEE", &
             "ANITRA", &
             "ALIDA", &
             "SIGRID", &
             "NICOLETTE", &
             "MARYJO", &
             "LINETTE", &
             "HEDWIG", &
             "CHRISTIANA", &
             "CASSIDY", &
             "ALEXIA", &
             "TRESSIE", &
             "MODESTA", &
             "LUPITA", &
             "LITA", &
             "GLADIS", &
             "EVELIA", &
             "DAVIDA", &
             "CHERRI", &
             "CECILY", &
             "ASHELY", &
             "ANNABEL", &
             "AGUSTINA", &
             "WANITA", &
             "SHIRLY", &
             "ROSAURA", &
             "HULDA", &
             "EUN", &
             "BAILEY", &
             "YETTA", &
             "VERONA", &
             "THOMASINA", &
             "SIBYL", &
             "SHANNAN", &
             "MECHELLE", &
             "LUE", &
             "LEANDRA", &
             "LANI", &
             "KYLEE", &
             "KANDY", &
             "JOLYNN", &
             "FERNE", &
             "EBONI", &
             "CORENE", &
             "ALYSIA", &
             "ZULA", &
             "NADA", &
             "MOIRA", &
             "LYNDSAY", &
             "LORRETTA", &
             "JUAN", &
             "JAMMIE", &
             "HORTENSIA", &
             "GAYNELL", &
             "CAMERON", &
             "ADRIA", &
             "VINA", &
             "VICENTA", &
             "TANGELA", &
             "STEPHINE", &
             "NORINE", &
             "NELLA", &
             "LIANA", &
             "LESLEE", &
             "KIMBERELY", &
             "ILIANA", &
             "GLORY", &
             "FELICA", &
             "EMOGENE", &
             "ELFRIEDE", &
             "EDEN", &
             "EARTHA", &
             "CARMA", &
             "BEA", &
             "OCIE", &
             "MARRY", &
             "LENNIE", &
             "KIARA", &
             "JACALYN", &
             "CARLOTA", &
             "ARIELLE", &
             "YU", &
             "STAR", &
             "OTILIA", &
             "KIRSTIN", &
             "KACEY", &
             "JOHNETTA", &
             "JOEY", &
             "JOETTA", &
             "JERALDINE", &
             "JAUNITA", &
             "ELANA", &
             "DORTHEA", &
             "CAMI", &
             "AMADA", &
             "ADELIA", &
             "VERNITA", &
             "TAMAR", &
             "SIOBHAN", &
             "RENEA", &
             "RASHIDA", &
             "OUIDA", &
             "ODELL", &
             "NILSA", &
             "MERYL", &
             "KRISTYN", &
             "JULIETA", &
             "DANICA", &
             "BREANNE", &
             "AUREA", &
             "ANGLEA", &
             "SHERRON", &
             "ODETTE", &
             "MALIA", &
             "LORELEI", &
             "LIN", &
             "LEESA", &
             "KENNA", &
             "KATHLYN", &
             "FIONA", &
             "CHARLETTE", &
             "SUZIE", &
             "SHANTELL", &
             "SABRA", &
             "RACQUEL", &
             "MYONG", &
             "MIRA", &
             "MARTINE", &
             "LUCIENNE", &
             "LAVADA", &
             "JULIANN", &
             "JOHNIE", &
             "ELVERA", &
             "DELPHIA", &
             "CLAIR", &
             "CHRISTIANE", &
             "CHAROLETTE", &
             "CARRI", &
             "AUGUSTINE", &
             "ASHA", &
             "ANGELLA", &
             "PAOLA", &
             "NINFA", &
             "LEDA", &
             "LAI", &
             "EDA", &
             "SUNSHINE", &
             "STEFANI", &
             "SHANELL", &
             "PALMA", &
             "MACHELLE", &
             "LISSA", &
             "KECIA", &
             "KATHRYNE", &
             "KARLENE", &
             "JULISSA", &
             "JETTIE", &
             "JENNIFFER", &
             "HUI", &
             "CORRINA", &
             "CHRISTOPHER", &
             "CAROLANN", &
             "ALENA", &
             "TESS", &
             "ROSARIA", &
             "MYRTICE", &
             "MARYLEE", &
             "LIANE", &
             "KENYATTA", &
             "JUDIE", &
             "JANEY", &
             "IN", &
             "ELMIRA", &
             "ELDORA", &
             "DENNA", &
             "CRISTI", &
             "CATHI", &
             "ZAIDA", &
             "VONNIE", &
             "VIVA", &
             "VERNIE", &
             "ROSALINE", &
             "MARIELA", &
             "LUCIANA", &
             "LESLI", &
             "KARAN", &
             "FELICE", &
             "DENEEN", &
             "ADINA", &
             "WYNONA", &
             "TARSHA", &
             "SHERON", &
             "SHASTA", &
             "SHANITA", &
             "SHANI", &
             "SHANDRA", &
             "RANDA", &
             "PINKIE", &
             "PARIS", &
             "NELIDA", &
             "MARILOU", &
             "LYLA", &
             "LAURENE", &
             "LACI", &
             "JOI", &
             "JANENE", &
             "DOROTHA", &
             "DANIELE", &
             "DANI", &
             "CAROLYNN", &
             "CARLYN", &
             "BERENICE", &
             "AYESHA", &
             "ANNELIESE", &
             "ALETHEA", &
             "THERSA", &
             "TAMIKO", &
             "RUFINA", &
             "OLIVA", &
             "MOZELL", &
             "MARYLYN", &
             "MADISON", &
             "KRISTIAN", &
             "KATHYRN", &
             "KASANDRA", &
             "KANDACE", &
             "JANAE", &
             "GABRIEL", &
             "DOMENICA", &
             "DEBBRA", &
             "DANNIELLE", &
             "CHUN", &
             "BUFFY", &
             "BARBIE", &
             "ARCELIA", &
             "AJA", &
             "ZENOBIA", &
             "SHAREN", &
             "SHAREE", &
             "PATRICK", &
             "PAGE", &
             "MY", &
             "LAVINIA", &
             "KUM", &
             "KACIE", &
             "JACKELINE", &
             "HUONG", &
             "FELISA", &
             "EMELIA", &
             "ELEANORA", &
             "CYTHIA", &
             "CRISTIN", &
             "CLYDE", &
             "CLARIBEL", &
             "CARON", &
             "ANASTACIA", &
             "ZULMA", &
             "ZANDRA", &
             "YOKO", &
             "TENISHA", &
             "SUSANN", &
             "SHERILYN", &
             "SHAY", &
             "SHAWANDA", &
             "SABINE", &
             "ROMANA", &
             "MATHILDA", &
             "LINSEY", &
             "KEIKO", &
             "JOANA", &
             "ISELA", &
             "GRETTA", &
             "GEORGETTA", &
             "EUGENIE", &
             "DUSTY", &
             "DESIRAE", &
             "DELORA", &
             "CORAZON", &
             "ANTONINA", &
             "ANIKA", &
             "WILLENE", &
             "TRACEE", &
             "TAMATHA", &
             "REGAN", &
             "NICHELLE", &
             "MICKIE", &
             "MAEGAN", &
             "LUANA", &
             "LANITA", &
             "KELSIE", &
             "EDELMIRA", &
             "BREE", &
             "AFTON", &
             "TEODORA", &
             "TAMIE", &
             "SHENA", &
             "MEG", &
             "LINH", &
             "KELI", &
             "KACI", &
             "DANYELLE", &
             "BRITT", &
             "ARLETTE", &
             "ALBERTINE", &
             "ADELLE", &
             "TIFFINY", &
             "STORMY", &
             "SIMONA", &
             "NUMBERS", &
             "NICOLASA", &
             "NICHOL", &
             "NIA", &
             "NAKISHA", &
             "MEE", &
             "MAIRA", &
             "LOREEN", &
             "KIZZY", &
             "JOHNNY", &
             "JAY", &
             "FALLON", &
             "CHRISTENE", &
             "BOBBYE", &
             "ANTHONY", &
             "YING", &
             "VINCENZA", &
             "TANJA", &
             "RUBIE", &
             "RONI", &
             "QUEENIE", &
             "MARGARETT", &
             "KIMBERLI", &
             "IRMGARD", &
             "IDELL", &
             "HILMA", &
             "EVELINA", &
             "ESTA", &
             "EMILEE", &
             "DENNISE", &
             "DANIA", &
             "CARL", &
             "CARIE", &
             "ANTONIO", &
             "WAI", &
             "SANG", &
             "RISA", &
             "RIKKI", &
             "PARTICIA", &
             "MUI", &
             "MASAKO", &
             "MARIO", &
             "LUVENIA", &
             "LOREE", &
             "LONI", &
             "LIEN", &
             "KEVIN", &
             "GIGI", &
             "FLORENCIA", &
             "DORIAN", &
             "DENITA", &
             "DALLAS", &
             "CHI", &
             "BILLYE", &
             "ALEXANDER", &
             "TOMIKA", &
             "SHARITA", &
             "RANA", &
             "NIKOLE", &
             "NEOMA", &
             "MARGARITE", &
             "MADALYN", &
             "LUCINA", &
             "LAILA", &
             "KALI", &
             "JENETTE", &
             "GABRIELE", &
             "EVELYNE", &
             "ELENORA", &
             "CLEMENTINA", &
             "ALEJANDRINA", &
             "ZULEMA", &
             "VIOLETTE", &
             "VANNESSA", &
             "THRESA", &
             "RETTA", &
             "PIA", &
             "PATIENCE", &
             "NOELLA", &
             "NICKIE", &
             "JONELL", &
             "DELTA", &
             "CHUNG", &
             "CHAYA", &
             "CAMELIA", &
             "BETHEL", &
             "ANYA", &
             "ANDREW", &
             "THANH", &
             "SUZANN", &
             "SPRING", &
             "SHU", &
             "MILA", &
             "LILLA", &
             "LAVERNA", &
             "KEESHA", &
             "KATTIE", &
             "GIA", &
             "GEORGENE", &
             "EVELINE", &
             "ESTELL", &
             "ELIZBETH", &
             "VIVIENNE", &
             "VALLIE", &
             "TRUDIE", &
             "STEPHANE", &
             "MICHEL", &
             "MAGALY", &
             "MADIE", &
             "KENYETTA", &
             "KARREN", &
             "JANETTA", &
             "HERMINE", &
             "HARMONY", &
             "DRUCILLA", &
             "DEBBI", &
             "CELESTINA", &
             "CANDIE", &
             "BRITNI", &
             "BECKIE", &
             "AMINA", &
             "ZITA", &
             "YUN", &
             "YOLANDE", &
             "VIVIEN", &
             "VERNETTA", &
             "TRUDI", &
             "SOMMER", &
             "PEARLE", &
             "PATRINA", &
             "OSSIE", &
             "NICOLLE", &
             "LOYCE", &
             "LETTY", &
             "LARISA", &
             "KATHARINA", &
             "JOSELYN", &
             "JONELLE", &
             "JENELL", &
             "IESHA", &
             "HEIDE", &
             "FLORINDA", &
             "FLORENTINA", &
             "FLO", &
             "ELODIA", &
             "DORINE", &
             "BRUNILDA", &
             "BRIGID", &
             "ASHLI", &
             "ARDELLA", &
             "TWANA", &
             "THU", &
             "TARAH", &
             "SUNG", &
             "SHEA", &
             "SHAVON", &
             "SHANE", &
             "SERINA", &
             "RAYNA", &
             "RAMONITA", &
             "NGA", &
             "MARGURITE", &
             "LUCRECIA", &
             "KOURTNEY", &
             "KATI", &
             "JESUS", &
             "JESENIA", &
             "DIAMOND", &
             "CRISTA", &
             "AYANA", &
             "ALICA", &
             "ALIA", &
             "VINNIE", &
             "SUELLEN", &
             "ROMELIA", &
             "RACHELL", &
             "PIPER", &
             "OLYMPIA", &
             "MICHIKO", &
             "KATHALEEN", &
             "JOLIE", &
             "JESSI", &
             "JANESSA", &
             "HANA", &
             "HA", &
             "ELEASE", &
             "CARLETTA", &
             "BRITANY", &
             "SHONA", &
             "SALOME", &
             "ROSAMOND", &
             "REGENA", &
             "RAINA", &
             "NGOC", &
             "NELIA", &
             "LOUVENIA", &
             "LESIA", &
             "LATRINA", &
             "LATICIA", &
             "LARHONDA", &
             "JINA", &
             "JACKI", &
             "HOLLIS", &
             "HOLLEY", &
             "EMMY", &
             "DEEANN", &
             "CORETTA", &
             "ARNETTA", &
             "VELVET", &
             "THALIA", &
             "SHANICE", &
             "NETA", &
             "MIKKI", &
             "MICKI", &
             "LONNA", &
             "LEANA", &
             "LASHUNDA", &
             "KILEY", &
             "JOYE", &
             "JACQULYN", &
             "IGNACIA", &
             "HYUN", &
             "HIROKO", &
             "HENRY", &
             "HENRIETTE", &
             "ELAYNE", &
             "DELINDA", &
             "DARNELL", &
             "DAHLIA", &
             "COREEN", &
             "CONSUELA", &
             "CONCHITA", &
             "CELINE", &
             "BABETTE", &
             "AYANNA", &
             "ANETTE", &
             "ALBERTINA", &
             "SKYE", &
             "SHAWNEE", &
             "SHANEKA", &
             "QUIANA", &
             "PAMELIA", &
             "MIN", &
             "MERRI", &
             "MERLENE", &
             "MARGIT", &
             "KIESHA", &
             "KIERA", &
             "KAYLENE", &
             "JODEE", &
             "JENISE", &
             "ERLENE", &
             "EMMIE", &
             "ELSE", &
             "DARYL", &
             "DALILA", &
             "DAISEY", &
             "CODY", &
             "CASIE", &
             "BELIA", &
             "BABARA", &
             "VERSIE", &
             "VANESA", &
             "SHELBA", &
             "SHAWNDA", &
             "SAM", &
             "NORMAN", &
             "NIKIA", &
             "NAOMA", &
             "MARNA", &
             "MARGERET", &
             "MADALINE", &
             "LAWANA", &
             "KINDRA", &
             "JUTTA", &
             "JAZMINE", &
             "JANETT", &
             "HANNELORE", &
             "GLENDORA", &
             "GERTRUD", &
             "GARNETT", &
             "FREEDA", &
             "FREDERICA", &
             "FLORANCE", &
             "FLAVIA", &
             "DENNIS", &
             "CARLINE", &
             "BEVERLEE", &
             "ANJANETTE", &
             "VALDA", &
             "TRINITY", &
             "TAMALA", &
             "STEVIE", &
             "SHONNA", &
             "SHA", &
             "SARINA", &
             "ONEIDA", &
             "MICAH", &
             "MERILYN", &
             "MARLEEN", &
             "LURLINE", &
             "LENNA", &
             "KATHERIN", &
             "JIN", &
             "JENI", &
             "HAE", &
             "GRACIA", &
             "GLADY", &
             "FARAH", &
             "ERIC", &
             "ENOLA", &
             "EMA", &
             "DOMINQUE", &
             "DEVONA", &
             "DELANA", &
             "CECILA", &
             "CAPRICE", &
             "ALYSHA", &
             "ALI", &
             "ALETHIA", &
             "VENA", &
             "THERESIA", &
             "TAWNY", &
             "SONG", &
             "SHAKIRA", &
             "SAMARA", &
             "SACHIKO", &
             "RACHELE", &
             "PAMELLA", &
             "NICKY", &
             "MARNI", &
             "MARIEL", &
             "MAREN", &
             "MALISA", &
             "LIGIA", &
             "LERA", &
             "LATORIA", &
             "LARAE", &
             "KIMBER", &
             "KATHERN", &
             "KAREY", &
             "JENNEFER", &
             "JANETH", &
             "HALINA", &
             "FREDIA", &
             "DELISA", &
             "DEBROAH", &
             "CIERA", &
             "CHIN", &
             "ANGELIKA", &
             "ANDREE", &
             "ALTHA", &
             "YEN", &
             "VIVAN", &
             "TERRESA", &
             "TANNA", &
             "SUK", &
             "SUDIE", &
             "SOO", &
             "SIGNE", &
             "SALENA", &
             "RONNI", &
             "REBBECCA", &
             "MYRTIE", &
             "MCKENZIE", &
             "MALIKA", &
             "MAIDA", &
             "LOAN", &
             "LEONARDA", &
             "KAYLEIGH", &
             "FRANCE", &
             "ETHYL", &
             "ELLYN", &
             "DAYLE", &
             "CAMMIE", &
             "BRITTNI", &
             "BIRGIT", &
             "AVELINA", &
             "ASUNCION", &
             "ARIANNA", &
             "AKIKO", &
             "VENICE", &
             "TYESHA", &
             "TONIE", &
             "TIESHA", &
             "TAKISHA", &
             "STEFFANIE", &
             "SINDY", &
             "SANTANA", &
             "MEGHANN", &
             "MANDA", &
             "MACIE", &
             "LADY", &
             "KELLYE", &
             "KELLEE", &
             "JOSLYN", &
             "JASON", &
             "INGER", &
             "INDIRA", &
             "GLINDA", &
             "GLENNIS", &
             "FERNANDA", &
             "FAUSTINA", &
             "ENEIDA", &
             "ELICIA", &
             "DOT", &
             "DIGNA", &
             "DELL", &
             "ARLETTA", &
             "ANDRE", &
             "WILLIA", &
             "TAMMARA", &
             "TABETHA", &
             "SHERRELL", &
             "SARI", &
             "REFUGIO", &
             "REBBECA", &
             "PAULETTA", &
             "NIEVES", &
             "NATOSHA", &
             "NAKITA", &
             "MAMMIE", &
             "KENISHA", &
             "KAZUKO", &
             "KASSIE", &
             "GARY", &
             "EARLEAN", &
             "DAPHINE", &
             "CORLISS", &
             "CLOTILDE", &
             "CAROLYNE", &
             "BERNETTA", &
             "AUGUSTINA", &
             "AUDREA", &
             "ANNIS", &
             "ANNABELL", &
             "YAN", &
             "TENNILLE", &
             "TAMICA", &
             "SELENE", &
             "SEAN", &
             "ROSANA", &
             "REGENIA", &
             "QIANA", &
             "MARKITA", &
             "MACY", &
             "LEEANNE", &
             "LAURINE", &
             "KYM", &
             "JESSENIA", &
             "JANITA", &
             "GEORGINE", &
             "GENIE", &
             "EMIKO", &
             "ELVIE", &
             "DEANDRA", &
             "DAGMAR", &
             "CORIE", &
             "COLLEN", &
             "CHERISH", &
             "ROMAINE", &
             "PORSHA", &
             "PEARLENE", &
             "MICHELINE", &
             "MERNA", &
             "MARGORIE", &
             "MARGARETTA", &
             "LORE", &
             "KENNETH", &
             "JENINE", &
             "HERMINA", &
             "FREDERICKA", &
             "ELKE", &
             "DRUSILLA", &
             "DORATHY", &
             "DIONE", &
             "DESIRE", &
             "CELENA", &
             "BRIGIDA", &
             "ANGELES", &
             "ALLEGRA", &
             "THEO", &
             "TAMEKIA", &
             "SYNTHIA", &
             "STEPHEN", &
             "SOOK", &
             "SLYVIA", &
             "ROSANN", &
             "REATHA", &
             "RAYE", &
             "MARQUETTA", &
             "MARGART", &
             "LING", &
             "LAYLA", &
             "KYMBERLY", &
             "KIANA", &
             "KAYLEEN", &
             "KATLYN", &
             "KARMEN", &
             "JOELLA", &
             "IRINA", &
             "EMELDA", &
             "ELENI", &
             "DETRA", &
             "CLEMMIE", &
             "CHERYLL", &
             "CHANTELL", &
             "CATHEY", &
             "ARNITA", &
             "ARLA", &
             "ANGLE", &
             "ANGELIC", &
             "ALYSE", &
             "ZOFIA", &
             "THOMASINE", &
             "TENNIE", &
             "SON", &
             "SHERLY", &
             "SHERLEY", &
             "SHARYL", &
             "REMEDIOS", &
             "PETRINA", &
             "NICKOLE", &
             "MYUNG", &
             "MYRLE", &
             "MOZELLA", &
             "LOUANNE", &
             "LISHA", &
             "LATIA", &
             "LANE", &
             "KRYSTA", &
             "JULIENNE", &
             "JOEL", &
             "JEANENE", &
             "JACQUALINE", &
             "ISAURA", &
             "GWENDA", &
             "EARLEEN", &
             "DONALD", &
             "CLEOPATRA", &
             "CARLIE", &
             "AUDIE", &
             "ANTONIETTA", &
             "ALISE", &
             "ALEX", &
             "VERDELL", &
             "VAL", &
             "TYLER", &
             "TOMOKO", &
             "THAO", &
             "TALISHA", &
             "STEVEN", &
             "SO", &
             "SHEMIKA", &
             "SHAUN", &
             "SCARLET", &
             "SAVANNA", &
             "SANTINA", &
             "ROSIA", &
             "RAEANN", &
             "ODILIA", &
             "NANA", &
             "MINNA", &
             "MAGAN", &
             "LYNELLE", &
             "LE", &
             "KARMA", &
             "JOEANN", &
             "IVANA", &
             "INELL", &
             "ILANA", &
             "HYE", &
             "HONEY", &
             "HEE", &
             "GUDRUN", &
             "FRANK", &
             "DREAMA", &
             "CRISSY", &
             "CHANTE", &
             "CARMELINA", &
             "ARVILLA", &
             "ARTHUR", &
             "ANNAMAE", &
             "ALVERA", &
             "ALEIDA", &
             "AARON", &
             "YEE", &
             "YANIRA", &
             "VANDA", &
             "TIANNA", &
             "TAM", &
             "STEFANIA", &
             "SHIRA", &
             "PERRY", &
             "NICOL", &
             "NANCIE", &
             "MONSERRATE", &
             "MINH", &
             "MELYNDA", &
             "MELANY", &
             "MATTHEW", &
             "LOVELLA", &
             "LAURE", &
             "KIRBY", &
             "KACY", &
             "JACQUELYNN", &
             "HYON", &
             "GERTHA", &
             "FRANCISCO", &
             "ELIANA", &
             "CHRISTENA", &
             "CHRISTEEN", &
             "CHARISE", &
             "CATERINA", &
             "CARLEY", &
             "CANDYCE", &
             "ARLENA", &
             "AMMIE", &
             "YANG", &
             "WILLETTE", &
             "VANITA", &
             "TUYET", &
             "TINY", &
             "SYREETA", &
             "SILVA", &
             "SCOTT", &
             "RONALD", &
             "PENNEY", &
             "NYLA", &
             "MICHAL", &
             "MAURICE", &
             "MARYAM", &
             "MARYA", &
             "MAGEN", &
             "LUDIE", &
             "LOMA", &
             "LIVIA", &
             "LANELL", &
             "KIMBERLIE", &
             "JULEE", &
             "DONETTA", &
             "DIEDRA", &
             "DENISHA", &
             "DEANE", &
             "DAWNE", &
             "CLARINE", &
             "CHERRYL", &
             "BRONWYN", &
             "BRANDON", &
             "ALLA", &
             "VALERY", &
             "TONDA", &
             "SUEANN", &
             "SORAYA", &
             "SHOSHANA", &
             "SHELA", &
             "SHARLEEN", &
             "SHANELLE", &
             "NERISSA", &
             "MICHEAL", &
             "MERIDITH", &
             "MELLIE", &
             "MAYE", &
             "MAPLE", &
             "MAGARET", &
             "LUIS", &
             "LILI", &
             "LEONILA", &
             "LEONIE", &
             "LEEANNA", &
             "LAVONIA", &
             "LAVERA", &
             "KRISTEL", &
             "KATHEY", &
             "KATHE", &
             "JUSTIN", &
             "JULIAN", &
             "JIMMY", &
             "JANN", &
             "ILDA", &
             "HILDRED", &
             "HILDEGARDE", &
             "GENIA", &
             "FUMIKO", &
             "EVELIN", &
             "ERMELINDA", &
             "ELLY", &
             "DUNG", &
             "DOLORIS", &
             "DIONNA", &
             "DANAE", &
             "BERNEICE", &
             "ANNICE", &
             "ALIX", &
             "VERENA", &
             "VERDIE", &
             "TRISTAN", &
             "SHAWNNA", &
             "SHAWANA", &
             "SHAUNNA", &
             "ROZELLA", &
             "RANDEE", &
             "RANAE", &
             "MILAGRO", &
             "LYNELL", &
             "LUISE", &
             "LOUIE", &
             "LOIDA", &
             "LISBETH", &
             "KARLEEN", &
             "JUNITA", &
             "JONA", &
             "ISIS", &
             "HYACINTH", &
             "HEDY", &
             "GWENN", &
             "ETHELENE", &
             "ERLINE", &
             "EDWARD", &
             "DONYA", &
             "DOMONIQUE", &
             "DELICIA", &
             "DANNETTE", &
             "CICELY", &
             "BRANDA", &
             "BLYTHE", &
             "BETHANN", &
             "ASHLYN", &
             "ANNALEE", &
             "ALLINE", &
             "YUKO", &
             "VELLA", &
             "TRANG", &
             "TOWANDA", &
             "TESHA", &
             "SHERLYN", &
             "NARCISA", &
             "MIGUELINA", &
             "MERI", &
             "MAYBELL", &
             "MARLANA", &
             "MARGUERITA", &
             "MADLYN", &
             "LUNA", &
             "LORY", &
             "LORIANN", &
             "LIBERTY", &
             "LEONORE", &
             "LEIGHANN", &
             "LAURICE", &
             "LATESHA", &
             "LARONDA", &
             "KATRICE", &
             "KASIE", &
             "KARL", &
             "KALEY", &
             "JADWIGA", &
             "GLENNIE", &
             "GEARLDINE", &
             "FRANCINA", &
             "EPIFANIA", &
             "DYAN", &
             "DORIE", &
             "DIEDRE", &
             "DENESE", &
             "DEMETRICE", &
             "DELENA", &
             "DARBY", &
             "CRISTIE", &
             "CLEORA", &
             "CATARINA", &
             "CARISA", &
             "BERNIE", &
             "BARBERA", &
             "ALMETA", &
             "TRULA", &
             "TEREASA", &
             "SOLANGE", &
             "SHEILAH", &
             "SHAVONNE", &
             "SANORA", &
             "ROCHELL", &
             "MATHILDE", &
             "MARGARETA", &
             "MAIA", &
             "LYNSEY", &
             "LAWANNA", &
             "LAUNA", &
             "KENA", &
             "KEENA", &
             "KATIA", &
             "JAMEY", &
             "GLYNDA", &
             "GAYLENE", &
             "ELVINA", &
             "ELANOR", &
             "DANUTA", &
             "DANIKA", &
             "CRISTEN", &
             "CORDIE", &
             "COLETTA", &
             "CLARITA", &
             "CARMON", &
             "BRYNN", &
             "AZUCENA", &
             "AUNDREA", &
             "ANGELE", &
             "YI", &
             "WALTER", &
             "VERLIE", &
             "VERLENE", &
             "TAMESHA", &
             "SILVANA", &
             "SEBRINA", &
             "SAMIRA", &
             "REDA", &
             "RAYLENE", &
             "PENNI", &
             "PANDORA", &
             "NORAH", &
             "NOMA", &
             "MIREILLE", &
             "MELISSIA", &
             "MARYALICE", &
             "LARAINE", &
             "KIMBERY", &
             "KARYL", &
             "KARINE", &
             "KAM", &
             "JOLANDA", &
             "JOHANA", &
             "JESUSA", &
             "JALEESA", &
             "JAE", &
             "JACQUELYNE", &
             "IRISH", &
             "ILUMINADA", &
             "HILARIA", &
             "HANH", &
             "GENNIE", &
             "FRANCIE", &
             "FLORETTA", &
             "EXIE", &
             "EDDA", &
             "DREMA", &
             "DELPHA", &
             "BEV", &
             "BARBAR", &
             "ASSUNTA", &
             "ARDELL", &
             "ANNALISA", &
             "ALISIA", &
             "YUKIKO", &
             "YOLANDO", &
             "WONDA", &
             "WEI", &
             "WALTRAUD", &
             "VETA", &
             "TEQUILA", &
             "TEMEKA", &
             "TAMEIKA", &
             "SHIRLEEN", &
             "SHENITA", &
             "PIEDAD", &
             "OZELLA", &
             "MIRTHA", &
             "MARILU", &
             "KIMIKO", &
             "JULIANE", &
             "JENICE", &
             "JEN", &
             "JANAY", &
             "JACQUILINE", &
             "HILDE", &
             "FE", &
             "FAE", &
             "EVAN", &
             "EUGENE", &
             "ELOIS", &
             "ECHO", &
             "DEVORAH", &
             "CHAU", &
             "BRINDA", &
             "BETSEY", &
             "ARMINDA", &
             "ARACELIS", &
             "APRYL", &
             "ANNETT", &
             "ALISHIA", &
             "VEOLA", &
             "USHA", &
             "TOSHIKO", &
             "THEOLA", &
             "TASHIA", &
             "TALITHA", &
             "SHERY", &
             "RUDY", &
             "RENETTA", &
             "REIKO", &
             "RASHEEDA", &
             "OMEGA", &
             "OBDULIA", &
             "MIKA", &
             "MELAINE", &
             "MEGGAN", &
             "MARTIN", &
             "MARLEN", &
             "MARGET", &
             "MARCELINE", &
             "MANA", &
             "MAGDALEN", &
             "LIBRADA", &
             "LEZLIE", &
             "LEXIE", &
             "LATASHIA", &
             "LASANDRA", &
             "KELLE", &
             "ISIDRA", &
             "ISA", &
             "INOCENCIA", &
             "GWYN", &
             "FRANCOISE", &
             "ERMINIA", &
             "ERINN", &
             "DIMPLE", &
             "DEVORA", &
             "CRISELDA", &
             "ARMANDA", &
             "ARIE", &
             "ARIANE", &
             "ANGELO", &
             "ANGELENA", &
             "ALLEN", &
             "ALIZA", &
             "ADRIENE", &
             "ADALINE", &
             "XOCHITL", &
             "TWANNA", &
             "TRAN", &
             "TOMIKO", &
             "TAMISHA", &
             "TAISHA", &
             "SUSY", &
             "SIU", &
             "RUTHA", &
             "ROXY", &
             "RHONA", &
             "RAYMOND", &
             "OTHA", &
             "NORIKO", &
             "NATASHIA", &
             "MERRIE", &
             "MELVIN", &
             "MARINDA", &
             "MARIKO", &
             "MARGERT", &
             "LORIS", &
             "LIZZETTE", &
             "LEISHA", &
             "KAILA", &
             "KA", &
             "JOANNIE", &
             "JERRICA", &
             "JENE", &
             "JANNET", &
             "JANEE", &
             "JACINDA", &
             "HERTA", &
             "ELENORE", &
             "DORETTA", &
             "DELAINE", &
             "DANIELL", &
             "CLAUDIE", &
             "CHINA", &
             "BRITTA", &
             "APOLONIA", &
             "AMBERLY", &
             "ALEASE", &
             "YURI", &
             "YUK", &
             "WEN", &
             "WANETA", &
             "UTE", &
             "TOMI", &
             "SHARRI", &
             "SANDIE", &
             "ROSELLE", &
             "REYNALDA", &
             "RAGUEL", &
             "PHYLICIA", &
             "PATRIA", &
             "OLIMPIA", &
             "ODELIA", &
             "MITZIE", &
             "MITCHELL", &
             "MISS", &
             "MINDA", &
             "MIGNON", &
             "MICA", &
             "MENDY", &
             "MARIVEL", &
             "MAILE", &
             "LYNETTA", &
             "LAVETTE", &
             "LAURYN", &
             "LATRISHA", &
             "LAKIESHA", &
             "KIERSTEN", &
             "KARY", &
             "JOSPHINE", &
             "JOLYN", &
             "JETTA", &
             "JANISE", &
             "JACQUIE", &
             "IVELISSE", &
             "GLYNIS", &
             "GIANNA", &
             "GAYNELLE", &
             "EMERALD", &
             "DEMETRIUS", &
             "DANYELL", &
             "DANILLE", &
             "DACIA", &
             "CORALEE", &
             "CHER", &
             "CEOLA", &
             "BRETT", &
             "BELL", &
             "ARIANNE", &
             "ALESHIA", &
             "YUNG", &
             "WILLIEMAE", &
             "TROY", &
             "TRINH", &
             "THORA", &
             "TAI", &
             "SVETLANA", &
             "SHERIKA", &
             "SHEMEKA", &
             "SHAUNDA", &
             "ROSELINE", &
             "RICKI", &
             "MELDA", &
             "MALLIE", &
             "LAVONNA", &
             "LATINA", &
             "LARRY", &
             "LAQUANDA", &
             "LALA", &
             "LACHELLE", &
             "KLARA", &
             "KANDIS", &
             "JOHNA", &
             "JEANMARIE", &
             "JAYE", &
             "HANG", &
             "GRAYCE", &
             "GERTUDE", &
             "EMERITA", &
             "EBONIE", &
             "CLORINDA", &
             "CHING", &
             "CHERY", &
             "CAROLA", &
             "BREANN", &
             "BLOSSOM", &
             "BERNARDINE", &
             "BECKI", &
             "ARLETHA", &
             "ARGELIA", &
             "ARA", &
             "ALITA", &
             "YULANDA", &
             "YON", &
             "YESSENIA", &
             "TOBI", &
             "TASIA", &
             "SYLVIE", &
             "SHIRL", &
             "SHIRELY", &
             "SHERIDAN", &
             "SHELLA", &
             "SHANTELLE", &
             "SACHA", &
             "ROYCE", &
             "REBECKA", &
             "REAGAN", &
             "PROVIDENCIA", &
             "PAULENE", &
             "MISHA", &
             "MIKI", &
             "MARLINE", &
             "MARICA", &
             "LORITA", &
             "LATOYIA", &
             "LASONYA", &
             "KERSTIN", &
             "KENDA", &
             "KEITHA", &
             "KATHRIN", &
             "JAYMIE", &
             "JACK", &
             "GRICELDA", &
             "GINETTE", &
             "ERYN", &
             "ELINA", &
             "ELFRIEDA", &
             "DANYEL", &
             "CHEREE", &
             "CHANELLE", &
             "BARRIE", &
             "AVERY", &
             "AURORE", &
             "ANNAMARIA", &
             "ALLEEN", &
             "AILENE", &
             "AIDE", &
             "YASMINE", &
             "VASHTI", &
             "VALENTINE", &
             "TREASA", &
             "TORY", &
             "TIFFANEY", &
             "SHERYLL", &
             "SHARIE", &
             "SHANAE", &
             "SAU", &
             "RAISA", &
             "PA", &
             "NEDA", &
             "MITSUKO", &
             "MIRELLA", &
             "MILDA", &
             "MARYANNA", &
             "MARAGRET", &
             "MABELLE", &
             "LUETTA", &
             "LORINA", &
             "LETISHA", &
             "LATARSHA", &
             "LANELLE", &
             "LAJUANA", &
             "KRISSY", &
             "KARLY", &
             "KARENA", &
             "JON", &
             "JESSIKA", &
             "JERICA", &
             "JEANELLE", &
             "JANUARY", &
             "JALISA", &
             "JACELYN", &
             "IZOLA", &
             "IVEY", &
             "GREGORY", &
             "EUNA", &
             "ETHA", &
             "DREW", &
             "DOMITILA", &
             "DOMINICA", &
             "DAINA", &
             "CREOLA", &
             "CARLI", &
             "CAMIE", &
             "BUNNY", &
             "BRITTNY", &
             "ASHANTI", &
             "ANISHA", &
             "ALEEN", &
             "ADAH", &
             "YASUKO", &
             "WINTER", &
             "VIKI", &
             "VALRIE", &
             "TONA", &
             "TINISHA", &
             "THI", &
             "TERISA", &
             "TATUM", &
             "TANEKA", &
             "SIMONNE", &
             "SHALANDA", &
             "SERITA", &
             "RESSIE", &
             "REFUGIA", &
             "PAZ", &
             "OLENE", &
             "NA", &
             "MERRILL", &
             "MARGHERITA", &
             "MANDIE", &
             "MAN", &
             "MAIRE", &
             "LYNDIA", &
             "LUCI", &
             "LORRIANE", &
             "LORETA", &
             "LEONIA", &
             "LAVONA", &
             "LASHAWNDA", &
             "LAKIA", &
             "KYOKO", &
             "KRYSTINA", &
             "KRYSTEN", &
             "KENIA", &
             "KELSI", &
             "JUDE", &
             "JEANICE", &
             "ISOBEL", &
             "GEORGIANN", &
             "GENNY", &
             "FELICIDAD", &
             "EILENE", &
             "DEON", &
             "DELOISE", &
             "DEEDEE", &
             "DANNIE", &
             "CONCEPTION", &
             "CLORA", &
             "CHERILYN", &
             "CHANG", &
             "CALANDRA", &
             "BERRY", &
             "ARMANDINA", &
             "ANISA", &
             "ULA", &
             "TIMOTHY", &
             "TIERA", &
             "THERESSA", &
             "STEPHANIA", &
             "SIMA", &
             "SHYLA", &
             "SHONTA", &
             "SHERA", &
             "SHAQUITA", &
             "SHALA", &
             "SAMMY", &
             "ROSSANA", &
             "NOHEMI", &
             "NERY", &
             "MORIAH", &
             "MELITA", &
             "MELIDA", &
             "MELANI", &
             "MARYLYNN", &
             "MARISHA", &
             "MARIETTE", &
             "MALORIE", &
             "MADELENE", &
             "LUDIVINA", &
             "LORIA", &
             "LORETTE", &
             "LORALEE", &
             "LIANNE", &
             "LEON", &
             "LAVENIA", &
             "LAURINDA", &
             "LASHON", &
             "KIT", &
             "KIMI", &
             "KEILA", &
             "KATELYNN", &
             "KAI", &
             "JONE", &
             "JOANE", &
             "JI", &
             "JAYNA", &
             "JANELLA", &
             "JA", &
             "HUE", &
             "HERTHA", &
             "FRANCENE", &
             "ELINORE", &
             "DESPINA", &
             "DELSIE", &
             "DEEDRA", &
             "CLEMENCIA", &
             "CARRY", &
             "CAROLIN", &
             "CARLOS", &
             "BULAH", &
             "BRITTANIE", &
             "BOK", &
             "BLONDELL", &
             "BIBI", &
             "BEAULAH", &
             "BEATA", &
             "ANNITA", &
             "AGRIPINA", &
             "VIRGEN", &
             "VALENE", &
             "UN", &
             "TWANDA", &
             "TOMMYE", &
             "TOI", &
             "TARRA", &
             "TARI", &
             "TAMMERA", &
             "SHAKIA", &
             "SADYE", &
             "RUTHANNE", &
             "ROCHEL", &
             "RIVKA", &
             "PURA", &
             "NENITA", &
             "NATISHA", &
             "MING", &
             "MERRILEE", &
             "MELODEE", &
             "MARVIS", &
             "LUCILLA", &
             "LEENA", &
             "LAVETA", &
             "LARITA", &
             "LANIE", &
             "KEREN", &
             "ILEEN", &
             "GEORGEANN", &
             "GENNA", &
             "GENESIS", &
             "FRIDA", &
             "EWA", &
             "EUFEMIA", &
             "EMELY", &
             "ELA", &
             "EDYTH", &
             "DEONNA", &
             "DEADRA", &
             "DARLENA", &
             "CHANELL", &
             "CHAN", &
             "CATHERN", &
             "CASSONDRA", &
             "CASSAUNDRA", &
             "BERNARDA", &
             "BERNA", &
             "ARLINDA", &
             "ANAMARIA", &
             "ALBERT", &
             "WESLEY", &
             "VERTIE", &
             "VALERI", &
             "TORRI", &
             "TATYANA", &
             "STASIA", &
             "SHERISE", &
             "SHERILL", &
             "SEASON", &
             "SCOTTIE", &
             "SANDA", &
             "RUTHE", &
             "ROSY", &
             "ROBERTO", &
             "ROBBI", &
             "RANEE", &
             "QUYEN", &
             "PEARLY", &
             "PALMIRA", &
             "ONITA", &
             "NISHA", &
             "NIESHA", &
             "NIDA", &
             "NEVADA", &
             "NAM", &
             "MERLYN", &
             "MAYOLA", &
             "MARYLOUISE", &
             "MARYLAND", &
             "MARX", &
             "MARTH", &
             "MARGENE", &
             "MADELAINE", &
             "LONDA", &
             "LEONTINE", &
             "LEOMA", &
             "LEIA", &
             "LAWRENCE", &
             "LAURALEE", &
             "LANORA", &
             "LAKITA", &
             "KIYOKO", &
             "KETURAH", &
             "KATELIN", &
             "KAREEN", &
             "JONIE", &
             "JOHNETTE", &
             "JENEE", &
             "JEANETT", &
             "IZETTA", &
             "HIEDI", &
             "HEIKE", &
             "HASSIE", &
             "HAROLD", &
             "GIUSEPPINA", &
             "GEORGANN", &
             "FIDELA", &
             "FERNANDE", &
             "ELWANDA", &
             "ELLAMAE", &
             "ELIZ", &
             "DUSTI", &
             "DOTTY", &
             "CYNDY", &
             "CORALIE", &
             "CELESTA", &
             "ARGENTINA", &
             "ALVERTA", &
             "XENIA", &
             "WAVA", &
             "VANETTA", &
             "TORRIE", &
             "TASHINA", &
             "TANDY", &
             "TAMBRA", &
             "TAMA", &
             "STEPANIE", &
             "SHILA", &
             "SHAUNTA", &
             "SHARAN", &
             "SHANIQUA", &
             "SHAE", &
             "SETSUKO", &
             "SERAFINA", &
             "SANDEE", &
             "ROSAMARIA", &
             "PRISCILA", &
             "OLINDA", &
             "NADENE", &
             "MUOI", &
             "MICHELINA", &
             "MERCEDEZ", &
             "MARYROSE", &
             "MARIN", &
             "MARCENE", &
             "MAO", &
             "MAGALI", &
             "MAFALDA", &
             "LOGAN", &
             "LINN", &
             "LANNIE", &
             "KAYCE", &
             "KAROLINE", &
             "KAMILAH", &
             "KAMALA", &
             "JUSTA", &
             "JOLINE", &
             "JENNINE", &
             "JACQUETTA", &
             "IRAIDA", &
             "GERALD", &
             "GEORGEANNA", &
             "FRANCHESCA", &
             "FAIRY", &
             "EMELINE", &
             "ELANE", &
             "EHTEL", &
             "EARLIE", &
             "DULCIE", &
             "DALENE", &
             "CRIS", &
             "CLASSIE", &
             "CHERE", &
             "CHARIS", &
             "CAROYLN", &
             "CARMINA", &
             "CARITA", &
             "BRIAN", &
             "BETHANIE", &
             "AYAKO", &
             "ARICA", &
             "AN", &
             "ALYSA", &
             "ALESSANDRA", &
             "AKILAH", &
             "ADRIEN", &
             "ZETTA", &
             "YOULANDA", &
             "YELENA", &
             "YAHAIRA", &
             "XUAN", &
             "WENDOLYN", &
             "VICTOR", &
             "TIJUANA", &
             "TERRELL", &
             "TERINA", &
             "TERESIA", &
             "SUZI", &
             "SUNDAY", &
             "SHERELL", &
             "SHAVONDA", &
             "SHAUNTE", &
             "SHARDA", &
             "SHAKITA", &
             "SENA", &
             "RYANN", &
             "RUBI", &
             "RIVA", &
             "REGINIA", &
             "REA", &
             "RACHAL", &
             "PARTHENIA", &
             "PAMULA", &
             "MONNIE", &
             "MONET", &
             "MICHAELE", &
             "MELIA", &
             "MARINE", &
             "MALKA", &
             "MAISHA", &
             "LISANDRA", &
             "LEO", &
             "LEKISHA", &
             "LEAN", &
             "LAURENCE", &
             "LAKENDRA", &
             "KRYSTIN", &
             "KORTNEY", &
             "KIZZIE", &
             "KITTIE", &
             "KERA", &
             "KENDAL", &
             "KEMBERLY", &
             "KANISHA", &
             "JULENE", &
             "JULE", &
             "JOSHUA", &
             "JOHANNE", &
             "JEFFREY", &
             "JAMEE", &
             "HAN", &
             "HALLEY", &
             "GIDGET", &
             "GALINA", &
             "FREDRICKA", &
             "FLETA", &
             "FATIMAH", &
             "EUSEBIA", &
             "ELZA", &
             "ELEONORE", &
             "DORTHEY", &
             "DORIA", &
             "DONELLA", &
             "DINORAH", &
             "DELORSE", &
             "CLARETHA", &
             "CHRISTINIA", &
             "CHARLYN", &
             "BONG", &
             "BELKIS", &
             "AZZIE", &
             "ANDERA", &
             "AIKO", &
             "ADENA", &
             "YER", &
             "YAJAIRA", &
             "WAN", &
             "VANIA", &
             "ULRIKE", &
             "TOSHIA", &
             "TIFANY", &
             "STEFANY", &
             "SHIZUE", &
             "SHENIKA", &
             "SHAWANNA", &
             "SHAROLYN", &
             "SHARILYN", &
             "SHAQUANA", &
             "SHANTAY", &
             "SEE", &
             "ROZANNE", &
             "ROSELEE", &
             "RICKIE", &
             "REMONA", &
             "REANNA", &
             "RAELENE", &
             "QUINN", &
             "PHUNG", &
             "PETRONILA", &
             "NATACHA", &
             "NANCEY", &
             "MYRL", &
             "MIYOKO", &
             "MIESHA", &
             "MERIDETH", &
             "MARVELLA", &
             "MARQUITTA", &
             "MARHTA", &
             "MARCHELLE", &
             "LIZETH", &
             "LIBBIE", &
             "LAHOMA", &
             "LADAWN", &
             "KINA", &
             "KATHELEEN", &
             "KATHARYN", &
             "KARISA", &
             "KALEIGH", &
             "JUNIE", &
             "JULIEANN", &
             "JOHNSIE", &
             "JANEAN", &
             "JAIMEE", &
             "JACKQUELINE", &
             "HISAKO", &
             "HERMA", &
             "HELAINE", &
             "GWYNETH", &
             "GLENN", &
             "GITA", &
             "EUSTOLIA", &
             "EMELINA", &
             "ELIN", &
             "EDRIS", &
             "DONNETTE", &
             "DONNETTA", &
             "DIERDRE", &
             "DENAE", &
             "DARCEL", &
             "CLAUDE", &
             "CLARISA", &
             "CINDERELLA", &
             "CHIA", &
             "CHARLESETTA", &
             "CHARITA", &
             "CELSA", &
             "CASSY", &
             "CASSI", &
             "CARLEE", &
             "BRUNA", &
             "BRITTANEY", &
             "BRANDE", &
             "BILLI", &
             "BAO", &
             "ANTONETTA", &
             "ANGLA", &
             "ANGELYN", &
             "ANALISA", &
             "ALANE", &
             "WENONA", &
             "WENDIE", &
             "VERONIQUE", &
             "VANNESA", &
             "TOBIE", &
             "TEMPIE", &
             "SUMIKO", &
             "SULEMA", &
             "SPARKLE", &
             "SOMER", &
             "SHEBA", &
             "SHAYNE", &
             "SHARICE", &
             "SHANEL", &
             "SHALON", &
             "SAGE", &
             "ROY", &
             "ROSIO", &
             "ROSELIA", &
             "RENAY", &
             "REMA", &
             "REENA", &
             "PORSCHE", &
             "PING", &
             "PEG", &
             "OZIE", &
             "ORETHA", &
             "ORALEE", &
             "ODA", &
             "NU", &
             "NGAN", &
             "NAKESHA", &
             "MILLY", &
             "MARYBELLE", &
             "MARLIN", &
             "MARIS", &
             "MARGRETT", &
             "MARAGARET", &
             "MANIE", &
             "LURLENE", &
             "LILLIA", &
             "LIESELOTTE", &
             "LAVELLE", &
             "LASHAUNDA", &
             "LAKEESHA", &
             "KEITH", &
             "KAYCEE", &
             "KALYN", &
             "JOYA", &
             "JOETTE", &
             "JENAE", &
             "JANIECE", &
             "ILLA", &
             "GRISEL", &
             "GLAYDS", &
             "GENEVIE", &
             "GALA", &
             "FREDDA", &
             "FRED", &
             "ELMER", &
             "ELEONOR", &
             "DEBERA", &
             "DEANDREA", &
             "DAN", &
             "CORRINNE", &
             "CORDIA", &
             "CONTESSA", &
             "COLENE", &
             "CLEOTILDE", &
             "CHARLOTT", &
             "CHANTAY", &
             "CECILLE", &
             "BEATRIS", &
             "AZALEE", &
             "ARLEAN", &
             "ARDATH", &
             "ANJELICA", &
             "ANJA", &
             "ALFREDIA", &
             "ALEISHA", &
             "ADAM", &
             "ZADA", &
             "YUONNE", &
             "XIAO", &
             "WILLODEAN", &
             "WHITLEY", &
             "VENNIE", &
             "VANNA", &
             "TYISHA", &
             "TOVA", &
             "TORIE", &
             "TONISHA", &
             "TILDA", &
             "TIEN", &
             "TEMPLE", &
             "SIRENA", &
             "SHERRIL", &
             "SHANTI", &
             "SHAN", &
             "SENAIDA", &
             "SAMELLA", &
             "ROBBYN", &
             "RENDA", &
             "REITA", &
             "PHEBE", &
             "PAULITA", &
             "NOBUKO", &
             "NGUYET", &
             "NEOMI", &
             "MOON", &
             "MIKAELA", &
             "MELANIA", &
             "MAXIMINA", &
             "MARG", &
             "MAISIE", &
             "LYNNA", &
             "LILLI", &
             "LAYNE", &
             "LASHAUN", &
             "LAKENYA", &
             "LAEL", &
             "KIRSTIE", &
             "KATHLINE", &
             "KASHA", &
             "KARLYN", &
             "KARIMA", &
             "JOVAN", &
             "JOSEFINE", &
             "JENNELL", &
             "JACQUI", &
             "JACKELYN", &
             "HYO", &
             "HIEN", &
             "GRAZYNA", &
             "FLORRIE", &
             "FLORIA", &
             "ELEONORA", &
             "DWANA", &
             "DORLA", &
             "DONG", &
             "DELMY", &
             "DEJA", &
             "DEDE", &
             "DANN", &
             "CRYSTA", &
             "CLELIA", &
             "CLARIS", &
             "CLARENCE", &
             "CHIEKO", &
             "CHERLYN", &
             "CHERELLE", &
             "CHARMAIN", &
             "CHARA", &
             "CAMMY", &
             "BEE", &
             "ARNETTE", &
             "ARDELLE", &
             "ANNIKA", &
             "AMIEE", &
             "AMEE", &
             "ALLENA", &
             "YVONE", &
             "YUKI", &
             "YOSHIE", &
             "YEVETTE", &
             "YAEL", &
             "WILLETTA", &
             "VONCILE", &
             "VENETTA", &
             "TULA", &
             "TONETTE", &
             "TIMIKA", &
             "TEMIKA", &
             "TELMA", &
             "TEISHA", &
             "TAREN", &
             "TA", &
             "STACEE", &
             "SHIN", &
             "SHAWNTA", &
             "SATURNINA", &
             "RICARDA", &
             "POK", &
             "PASTY", &
             "ONIE", &
             "NUBIA", &
             "MORA", &
             "MIKE", &
             "MARIELLE", &
             "MARIELLA", &
             "MARIANELA", &
             "MARDELL", &
             "MANY", &
             "LUANNA", &
             "LOISE", &
             "LISABETH", &
             "LINDSY", &
             "LILLIANA", &
             "LILLIAM", &
             "LELAH", &
             "LEIGHA", &
             "LEANORA", &
             "LANG", &
             "KRISTEEN", &
             "KHALILAH", &
             "KEELEY", &
             "KANDRA", &
             "JUNKO", &
             "JOAQUINA", &
             "JERLENE", &
             "JANI", &
             "JAMIKA", &
             "JAME", &
             "HSIU", &
             "HERMILA", &
             "GOLDEN", &
             "GENEVIVE", &
             "EVIA", &
             "EUGENA", &
             "EMMALINE", &
             "ELFREDA", &
             "ELENE", &
             "DONETTE", &
             "DELCIE", &
             "DEEANNA", &
             "DARCEY", &
             "CUC", &
             "CLARINDA", &
             "CIRA", &
             "CHAE", &
             "CELINDA", &
             "CATHERYN", &
             "CATHERIN", &
             "CASIMIRA", &
             "CARMELIA", &
             "CAMELLIA", &
             "BREANA", &
             "BOBETTE", &
             "BERNARDINA", &
             "BEBE", &
             "BASILIA", &
             "ARLYNE", &
             "AMAL", &
             "ALAYNA", &
             "ZONIA", &
             "ZENIA", &
             "YURIKO", &
             "YAEKO", &
             "WYNELL", &
             "WILLOW", &
             "WILLENA", &
             "VERNIA", &
             "TU", &
             "TRAVIS", &
             "TORA", &
             "TERRILYN", &
             "TERICA", &
             "TENESHA", &
             "TAWNA", &
             "TAJUANA", &
             "TAINA", &
             "STEPHNIE", &
             "SONA", &
             "SOL", &
             "SINA", &
             "SHONDRA", &
             "SHIZUKO", &
             "SHERLENE", &
             "SHERICE", &
             "SHARIKA", &
             "ROSSIE", &
             "ROSENA", &
             "RORY", &
             "RIMA", &
             "RIA", &
             "RHEBA", &
             "RENNA", &
             "PETER", &
             "NATALYA", &
             "NANCEE", &
             "MELODI", &
             "MEDA", &
             "MAXIMA", &
             "MATHA", &
             "MARKETTA", &
             "MARICRUZ", &
             "MARCELENE", &
             "MALVINA", &
             "LUBA", &
             "LOUETTA", &
             "LEIDA", &
             "LECIA", &
             "LAURAN", &
             "LASHAWNA", &
             "LAINE", &
             "KHADIJAH", &
             "KATERINE", &
             "KASI", &
             "KALLIE", &
             "JULIETTA", &
             "JESUSITA", &
             "JESTINE", &
             "JESSIA", &
             "JEREMY", &
             "JEFFIE", &
             "JANYCE", &
             "ISADORA", &
             "GEORGIANNE", &
             "FIDELIA", &
             "EVITA", &
             "EURA", &
             "EULAH", &
             "ESTEFANA", &
             "ELSY", &
             "ELIZABET", &
             "ELADIA", &
             "DODIE", &
             "DION", &
             "DIA", &
             "DENISSE", &
             "DELORAS", &
             "DELILA", &
             "DAYSI", &
             "DAKOTA", &
             "CURTIS", &
             "CRYSTLE", &
             "CONCHA", &
             "COLBY", &
             "CLARETTA", &
             "CHU", &
             "CHRISTIA", &
             "CHARLSIE", &
             "CHARLENA", &
             "CARYLON", &
             "BETTYANN", &
             "ASLEY", &
             "ASHLEA", &
             "AMIRA", &
             "AI", &
             "AGUEDA", &
             "AGNUS", &
             "YUETTE", &
             "VINITA", &
             "VICTORINA", &
             "TYNISHA", &
             "TREENA", &
             "TOCCARA", &
             "TISH", &
             "THOMASENA", &
             "TEGAN", &
             "SOILA", &
             "SHILOH", &
             "SHENNA", &
             "SHARMAINE", &
             "SHANTAE", &
             "SHANDI", &
             "SEPTEMBER", &
             "SARAN", &
             "SARAI", &
             "SANA", &
             "SAMUEL", &
             "SALLEY", &
             "ROSETTE", &
             "ROLANDE", &
             "REGINE", &
             "OTELIA", &
             "OSCAR", &
             "OLEVIA", &
             "NICHOLLE", &
             "NECOLE", &
             "NAIDA", &
             "MYRTA", &
             "MYESHA", &
             "MITSUE", &
             "MINTA", &
             "MERTIE", &
             "MARGY", &
             "MAHALIA", &
             "MADALENE", &
             "LOVE", &
             "LOURA", &
             "LOREAN", &
             "LEWIS", &
             "LESHA", &
             "LEONIDA", &
             "LENITA", &
             "LAVONE", &
             "LASHELL", &
             "LASHANDRA", &
             "LAMONICA", &
             "KIMBRA", &
             "KATHERINA", &
             "KARRY", &
             "KANESHA", &
             "JULIO", &
             "JONG", &
             "JENEVA", &
             "JAQUELYN", &
             "HWA", &
             "GILMA", &
             "GHISLAINE", &
             "GERTRUDIS", &
             "FRANSISCA", &
             "FERMINA", &
             "ETTIE", &
             "ETSUKO", &
             "ELLIS", &
             "ELLAN", &
             "ELIDIA", &
             "EDRA", &
             "DORETHEA", &
             "DOREATHA", &
             "DENYSE", &
             "DENNY", &
             "DEETTA", &
             "DAINE", &
             "CYRSTAL", &
             "CORRIN", &
             "CAYLA", &
             "CARLITA", &
             "CAMILA", &
             "BURMA", &
             "BULA", &
             "BUENA", &
             "BLAKE", &
             "BARABARA", &
             "AVRIL", &
             "AUSTIN", &
             "ALAINE", &
             "ZANA", &
             "WILHEMINA", &
             "WANETTA", &
             "VIRGIL", &
             "VI", &
             "VERONIKA", &
             "VERNON", &
             "VERLINE", &
             "VASILIKI", &
             "TONITA", &
             "TISA", &
             "TEOFILA", &
             "TAYNA", &
             "TAUNYA", &
             "TANDRA", &
             "TAKAKO", &
             "SUNNI", &
             "SUANNE", &
             "SIXTA", &
             "SHARELL", &
             "SEEMA", &
             "RUSSELL", &
             "ROSENDA", &
             "ROBENA", &
             "RAYMONDE", &
             "PEI", &
             "PAMILA", &
             "OZELL", &
             "NEIDA", &
             "NEELY", &
             "MISTIE", &
             "MICHA", &
             "MERISSA", &
             "MAURITA", &
             "MARYLN", &
             "MARYETTA", &
             "MARSHALL", &
             "MARCELL", &
             "MALENA", &
             "MAKEDA", &
             "MADDIE", &
             "LOVETTA", &
             "LOURIE", &
             "LORRINE", &
             "LORILEE", &
             "LESTER", &
             "LAURENA", &
             "LASHAY", &
             "LARRAINE", &
             "LAREE", &
             "LACRESHA", &
             "KRISTLE", &
             "KRISHNA", &
             "KEVA", &
             "KEIRA", &
             "KAROLE", &
             "JOIE", &
             "JINNY", &
             "JEANNETTA", &
             "JAMA", &
             "HEIDY", &
             "GILBERTE", &
             "GEMA", &
             "FAVIOLA", &
             "EVELYNN", &
             "ENDA", &
             "ELLI", &
             "ELLENA", &
             "DIVINA", &
             "DAGNY", &
             "COLLENE", &
             "CODI", &
             "CINDIE", &
             "CHASSIDY", &
             "CHASIDY", &
             "CATRICE", &
             "CATHERINA", &
             "CASSEY", &
             "CAROLL", &
             "CARLENA", &
             "CANDRA", &
             "CALISTA", &
             "BRYANNA", &
             "BRITTENY", &
             "BEULA", &
             "BARI", &
             "AUDRIE", &
             "AUDRIA", &
             "ARDELIA", &
             "ANNELLE", &
             "ANGILA", &
             "ALONA", &
             "ALLYN", &
             "DOUGLAS", &
             "ROGER", &
             "JONATHAN", &
             "RALPH", &
             "NICHOLAS", &
             "BENJAMIN", &
             "BRUCE", &
             "HARRY", &
             "WAYNE", &
             "STEVE", &
             "HOWARD", &
             "ERNEST", &
             "PHILLIP", &
             "TODD", &
             "CRAIG", &
             "ALAN", &
             "PHILIP", &
             "EARL", &
             "DANNY", &
             "BRYAN", &
             "STANLEY", &
             "LEONARD", &
             "NATHAN", &
             "MANUEL", &
             "RODNEY", &
             "MARVIN", &
             "VINCENT", &
             "JEFFERY", &
             "JEFF", &
             "CHAD", &
             "JACOB", &
             "ALFRED", &
             "BRADLEY", &
             "HERBERT", &
             "FREDERICK", &
             "EDWIN", &
             "DON", &
             "RICKY", &
             "RANDALL", &
             "BARRY", &
             "BERNARD", &
             "LEROY", &
             "MARCUS", &
             "THEODORE", &
             "CLIFFORD", &
             "MIGUEL", &
             "JIM", &
             "TOM", &
             "CALVIN", &
             "BILL", &
             "LLOYD", &
             "DEREK", &
             "WARREN", &
             "DARRELL", &
             "JEROME", &
             "FLOYD", &
             "ALVIN", &
             "TIM", &
             "GORDON", &
             "GREG", &
             "JORGE", &
             "DUSTIN", &
             "PEDRO", &
             "DERRICK", &
             "ZACHARY", &
             "HERMAN", &
             "GLEN", &
             "HECTOR", &
             "RICARDO", &
             "RICK", &
             "BRENT", &
             "RAMON", &
             "GILBERT", &
             "MARC", &
             "REGINALD", &
             "RUBEN", &
             "NATHANIEL", &
             "RAFAEL", &
             "EDGAR", &
             "MILTON", &
             "RAUL", &
             "BEN", &
             "CHESTER", &
             "DUANE", &
             "FRANKLIN", &
             "BRAD", &
             "RON", &
             "ROLAND", &
             "ARNOLD", &
             "HARVEY", &
             "JARED", &
             "ERIK", &
             "DARRYL", &
             "NEIL", &
             "JAVIER", &
             "FERNANDO", &
             "CLINTON", &
             "TED", &
             "MATHEW", &
             "TYRONE", &
             "DARREN", &
             "LANCE", &
             "KURT", &
             "ALLAN", &
             "NELSON", &
             "GUY", &
             "CLAYTON", &
             "HUGH", &
             "MAX", &
             "DWAYNE", &
             "DWIGHT", &
             "ARMANDO", &
             "FELIX", &
             "EVERETT", &
             "IAN", &
             "WALLACE", &
             "KEN", &
             "BOB", &
             "ALFREDO", &
             "ALBERTO", &
             "DAVE", &
             "IVAN", &
             "BYRON", &
             "ISAAC", &
             "MORRIS", &
             "CLIFTON", &
             "WILLARD", &
             "ROSS", &
             "ANDY", &
             "SALVADOR", &
             "KIRK", &
             "SERGIO", &
             "SETH", &
             "KENT", &
             "TERRANCE", &
             "EDUARDO", &
             "TERRENCE", &
             "ENRIQUE", &
             "WADE", &
             "STUART", &
             "FREDRICK", &
             "ARTURO", &
             "ALEJANDRO", &
             "NICK", &
             "LUTHER", &
             "WENDELL", &
             "JEREMIAH", &
             "JULIUS", &
             "OTIS", &
             "TREVOR", &
             "OLIVER", &
             "LUKE", &
             "HOMER", &
             "GERARD", &
             "DOUG", &
             "KENNY", &
             "HUBERT", &
             "LYLE", &
             "MATT", &
             "ALFONSO", &
             "ORLANDO", &
             "REX", &
             "CARLTON", &
             "ERNESTO", &
             "NEAL", &
             "PABLO", &
             "LORENZO", &
             "OMAR", &
             "WILBUR", &
             "GRANT", &
             "HORACE", &
             "RODERICK", &
             "ABRAHAM", &
             "WILLIS", &
             "RICKEY", &
             "ANDRES", &
             "CESAR", &
             "JOHNATHAN", &
             "MALCOLM", &
             "RUDOLPH", &
             "DAMON", &
             "KELVIN", &
             "PRESTON", &
             "ALTON", &
             "ARCHIE", &
             "MARCO", &
             "WM", &
             "PETE", &
             "RANDOLPH", &
             "GARRY", &
             "GEOFFREY", &
             "JONATHON", &
             "FELIPE", &
             "GERARDO", &
             "ED", &
             "DOMINIC", &
             "DELBERT", &
             "COLIN", &
             "GUILLERMO", &
             "EARNEST", &
             "LUCAS", &
             "BENNY", &
             "SPENCER", &
             "RODOLFO", &
             "MYRON", &
             "EDMUND", &
             "GARRETT", &
             "SALVATORE", &
             "CEDRIC", &
             "LOWELL", &
             "GREGG", &
             "SHERMAN", &
             "WILSON", &
             "SYLVESTER", &
             "ROOSEVELT", &
             "ISRAEL", &
             "JERMAINE", &
             "FORREST", &
             "WILBERT", &
             "LELAND", &
             "SIMON", &
             "CLARK", &
             "IRVING", &
             "BRYANT", &
             "OWEN", &
             "RUFUS", &
             "WOODROW", &
             "KRISTOPHER", &
             "MACK", &
             "LEVI", &
             "MARCOS", &
             "GUSTAVO", &
             "JAKE", &
             "LIONEL", &
             "GILBERTO", &
             "CLINT", &
             "NICOLAS", &
             "ISMAEL", &
             "ORVILLE", &
             "ERVIN", &
             "DEWEY", &
             "AL", &
             "WILFRED", &
             "JOSH", &
             "HUGO", &
             "IGNACIO", &
             "CALEB", &
             "TOMAS", &
             "SHELDON", &
             "ERICK", &
             "STEWART", &
             "DOYLE", &
             "DARREL", &
             "ROGELIO", &
             "TERENCE", &
             "SANTIAGO", &
             "ALONZO", &
             "ELIAS", &
             "BERT", &
             "ELBERT", &
             "RAMIRO", &
             "CONRAD", &
             "NOAH", &
             "GRADY", &
             "PHIL", &
             "CORNELIUS", &
             "LAMAR", &
             "ROLANDO", &
             "CLAY", &
             "PERCY", &
             "DEXTER", &
             "BRADFORD", &
             "DARIN", &
             "AMOS", &
             "MOSES", &
             "IRVIN", &
             "SAUL", &
             "ROMAN", &
             "RANDAL", &
             "TIMMY", &
             "DARRIN", &
             "WINSTON", &
             "BRENDAN", &
             "ABEL", &
             "DOMINICK", &
             "BOYD", &
             "EMILIO", &
             "ELIJAH", &
             "DOMINGO", &
             "EMMETT", &
             "MARLON", &
             "EMANUEL", &
             "JERALD", &
             "EDMOND", &
             "EMIL", &
             "DEWAYNE", &
             "WILL", &
             "OTTO", &
             "TEDDY", &
             "REYNALDO", &
             "BRET", &
             "JESS", &
             "TRENT", &
             "HUMBERTO", &
             "EMMANUEL", &
             "STEPHAN", &
             "VICENTE", &
             "LAMONT", &
             "GARLAND", &
             "MILES", &
             "EFRAIN", &
             "HEATH", &
             "RODGER", &
             "HARLEY", &
             "ETHAN", &
             "ELDON", &
             "ROCKY", &
             "PIERRE", &
             "JUNIOR", &
             "FREDDY", &
             "ELI", &
             "BRYCE", &
             "ANTOINE", &
             "STERLING", &
             "CHASE", &
             "GROVER", &
             "ELTON", &
             "CLEVELAND", &
             "DYLAN", &
             "CHUCK", &
             "DAMIAN", &
             "REUBEN", &
             "STAN", &
             "AUGUST", &
             "LEONARDO", &
             "JASPER", &
             "RUSSEL", &
             "ERWIN", &
             "BENITO", &
             "HANS", &
             "MONTE", &
             "BLAINE", &
             "ERNIE", &
             "CURT", &
             "QUENTIN", &
             "AGUSTIN", &
             "MURRAY", &
             "JAMAL", &
             "ADOLFO", &
             "HARRISON", &
             "TYSON", &
             "BURTON", &
             "BRADY", &
             "ELLIOTT", &
             "WILFREDO", &
             "BART", &
             "JARROD", &
             "VANCE", &
             "DENIS", &
             "DAMIEN", &
             "JOAQUIN", &
             "HARLAN", &
             "DESMOND", &
             "ELLIOT", &
             "DARWIN", &
             "GREGORIO", &
             "BUDDY", &
             "XAVIER", &
             "KERMIT", &
             "ROSCOE", &
             "ESTEBAN", &
             "ANTON", &
             "SOLOMON", &
             "SCOTTY", &
             "NORBERT", &
             "ELVIN", &
             "WILLIAMS", &
             "NOLAN", &
             "ROD", &
             "QUINTON", &
             "HAL", &
             "BRAIN", &
             "ROB", &
             "ELWOOD", &
             "KENDRICK", &
             "DARIUS", &
             "MOISES", &
             "FIDEL", &
             "THADDEUS", &
             "CLIFF", &
             "MARCEL", &
             "JACKSON", &
             "RAPHAEL", &
             "BRYON", &
             "ARMAND", &
             "ALVARO", &
             "JEFFRY", &
             "DANE", &
             "JOESPH", &
             "THURMAN", &
             "NED", &
             "RUSTY", &
             "MONTY", &
             "FABIAN", &
             "REGGIE", &
             "MASON", &
             "GRAHAM", &
             "ISAIAH", &
             "VAUGHN", &
             "GUS", &
             "LOYD", &
             "DIEGO", &
             "ADOLPH", &
             "NORRIS", &
             "MILLARD", &
             "ROCCO", &
             "GONZALO", &
             "DERICK", &
             "RODRIGO", &
             "WILEY", &
             "RIGOBERTO", &
             "ALPHONSO", &
             "TY", &
             "NOE", &
             "VERN", &
             "REED", &
             "JEFFERSON", &
             "ELVIS", &
             "BERNARDO", &
             "MAURICIO", &
             "HIRAM", &
             "DONOVAN", &
             "BASIL", &
             "RILEY", &
             "NICKOLAS", &
             "MAYNARD", &
             "SCOT", &
             "VINCE", &
             "QUINCY", &
             "EDDY", &
             "SEBASTIAN", &
             "FEDERICO", &
             "ULYSSES", &
             "HERIBERTO", &
             "DONNELL", &
             "COLE", &
             "DAVIS", &
             "GAVIN", &
             "EMERY", &
             "WARD", &
             "ROMEO", &
             "JAYSON", &
             "DANTE", &
             "CLEMENT", &
             "COY", &
             "MAXWELL", &
             "JARVIS", &
             "BRUNO", &
             "ISSAC", &
             "DUDLEY", &
             "BROCK", &
             "SANFORD", &
             "CARMELO", &
             "BARNEY", &
             "NESTOR", &
             "STEFAN", &
             "DONNY", &
             "ART", &
             "LINWOOD", &
             "BEAU", &
             "WELDON", &
             "GALEN", &
             "ISIDRO", &
             "TRUMAN", &
             "DELMAR", &
             "JOHNATHON", &
             "SILAS", &
             "FREDERIC", &
             "DICK", &
             "IRWIN", &
             "MERLIN", &
             "CHARLEY", &
             "MARCELINO", &
             "HARRIS", &
             "CARLO", &
             "TRENTON", &
             "KURTIS", &
             "HUNTER", &
             "AURELIO", &
             "WINFRED", &
             "VITO", &
             "COLLIN", &
             "DENVER", &
             "CARTER", &
             "LEONEL", &
             "EMORY", &
             "PASQUALE", &
             "MOHAMMAD", &
             "MARIANO", &
             "DANIAL", &
             "LANDON", &
             "DIRK", &
             "BRANDEN", &
             "ADAN", &
             "BUFORD", &
             "GERMAN", &
             "WILMER", &
             "EMERSON", &
             "ZACHERY", &
             "FLETCHER", &
             "JACQUES", &
             "ERROL", &
             "DALTON", &
             "MONROE", &
             "JOSUE", &
             "EDWARDO", &
             "BOOKER", &
             "WILFORD", &
             "SONNY", &
             "SHELTON", &
             "CARSON", &
             "THERON", &
             "RAYMUNDO", &
             "DAREN", &
             "HOUSTON", &
             "ROBBY", &
             "LINCOLN", &
             "GENARO", &
             "BENNETT", &
             "OCTAVIO", &
             "CORNELL", &
             "HUNG", &
             "ARRON", &
             "ANTONY", &
             "HERSCHEL", &
             "GIOVANNI", &
             "GARTH", &
             "CYRUS", &
             "CYRIL", &
             "RONNY", &
             "LON", &
             "FREEMAN", &
             "DUNCAN", &
             "KENNITH", &
             "CARMINE", &
             "ERICH", &
             "CHADWICK", &
             "WILBURN", &
             "RUSS", &
             "REID", &
             "MYLES", &
             "ANDERSON", &
             "MORTON", &
             "JONAS", &
             "FOREST", &
             "MITCHEL", &
             "MERVIN", &
             "ZANE", &
             "RICH", &
             "JAMEL", &
             "LAZARO", &
             "ALPHONSE", &
             "RANDELL", &
             "MAJOR", &
             "JARRETT", &
             "BROOKS", &
             "ABDUL", &
             "LUCIANO", &
             "SEYMOUR", &
             "EUGENIO", &
             "MOHAMMED", &
             "VALENTIN", &
             "CHANCE", &
             "ARNULFO", &
             "LUCIEN", &
             "FERDINAND", &
             "THAD", &
             "EZRA", &
             "ALDO", &
             "RUBIN", &
             "ROYAL", &
             "MITCH", &
             "EARLE", &
             "ABE", &
             "WYATT", &
             "MARQUIS", &
             "LANNY", &
             "KAREEM", &
             "JAMAR", &
             "BORIS", &
             "ISIAH", &
             "EMILE", &
             "ELMO", &
             "ARON", &
             "LEOPOLDO", &
             "EVERETTE", &
             "JOSEF", &
             "ELOY", &
             "RODRICK", &
             "REINALDO", &
             "LUCIO", &
             "JERROD", &
             "WESTON", &
             "HERSHEL", &
             "BARTON", &
             "PARKER", &
             "LEMUEL", &
             "BURT", &
             "JULES", &
             "GIL", &
             "ELISEO", &
             "AHMAD", &
             "NIGEL", &
             "EFREN", &
             "ANTWAN", &
             "ALDEN", &
             "MARGARITO", &
             "COLEMAN", &
             "DINO", &
             "OSVALDO", &
             "LES", &
             "DEANDRE", &
             "NORMAND", &
             "KIETH", &
             "TREY", &
             "NORBERTO", &
             "NAPOLEON", &
             "JEROLD", &
             "FRITZ", &
             "ROSENDO", &
             "MILFORD", &
             "CHRISTOPER", &
             "ALFONZO", &
             "LYMAN", &
             "JOSIAH", &
             "BRANT", &
             "WILTON", &
             "RICO", &
             "JAMAAL", &
             "DEWITT", &
             "BRENTON", &
             "OLIN", &
             "FOSTER", &
             "FAUSTINO", &
             "CLAUDIO", &
             "JUDSON", &
             "GINO", &
             "EDGARDO", &
             "ALEC", &
             "TANNER", &
             "JARRED", &
             "DONN", &
             "TAD", &
             "PRINCE", &
             "PORFIRIO", &
             "ODIS", &
             "LENARD", &
             "CHAUNCEY", &
             "TOD", &
             "MEL", &
             "MARCELO", &
             "KORY", &
             "AUGUSTUS", &
             "KEVEN", &
             "HILARIO", &
             "BUD", &
             "SAL", &
             "ORVAL", &
             "MAURO", &
             "ZACHARIAH", &
             "OLEN", &
             "ANIBAL", &
             "MILO", &
             "JED", &
             "DILLON", &
             "AMADO", &
             "NEWTON", &
             "LENNY", &
             "RICHIE", &
             "HORACIO", &
             "BRICE", &
             "MOHAMED", &
             "DELMER", &
             "DARIO", &
             "REYES", &
             "MAC", &
             "JONAH", &
             "JERROLD", &
             "ROBT", &
             "HANK", &
             "RUPERT", &
             "ROLLAND", &
             "KENTON", &
             "DAMION", &
             "ANTONE", &
             "WALDO", &
             "FREDRIC", &
             "BRADLY", &
             "KIP", &
             "BURL", &
             "WALKER", &
             "TYREE", &
             "JEFFEREY", &
             "AHMED", &
             "WILLY", &
             "STANFORD", &
             "OREN", &
             "NOBLE", &
             "MOSHE", &
             "MIKEL", &
             "ENOCH", &
             "BRENDON", &
             "QUINTIN", &
             "JAMISON", &
             "FLORENCIO", &
             "DARRICK", &
             "TOBIAS", &
             "HASSAN", &
             "GIUSEPPE", &
             "DEMARCUS", &
             "CLETUS", &
             "TYRELL", &
             "LYNDON", &
             "KEENAN", &
             "WERNER", &
             "GERALDO", &
             "COLUMBUS", &
             "CHET", &
             "BERTRAM", &
             "MARKUS", &
             "HUEY", &
             "HILTON", &
             "DWAIN", &
             "DONTE", &
             "TYRON", &
             "OMER", &
             "ISAIAS", &
             "HIPOLITO", &
             "FERMIN", &
             "ADALBERTO", &
             "BO", &
             "BARRETT", &
             "TEODORO", &
             "MCKINLEY", &
             "MAXIMO", &
             "GARFIELD", &
             "RALEIGH", &
             "LAWERENCE", &
             "ABRAM", &
             "RASHAD", &
             "KING", &
             "EMMITT", &
             "DARON", &
             "SAMUAL", &
             "MIQUEL", &
             "EUSEBIO", &
             "DOMENIC", &
             "DARRON", &
             "BUSTER", &
             "WILBER", &
             "RENATO", &
             "JC", &
             "HOYT", &
             "HAYWOOD", &
             "EZEKIEL", &
             "CHAS", &
             "FLORENTINO", &
             "ELROY", &
             "CLEMENTE", &
             "ARDEN", &
             "NEVILLE", &
             "EDISON", &
             "DESHAWN", &
             "NATHANIAL", &
             "JORDON", &
             "DANILO", &
             "CLAUD", &
             "SHERWOOD", &
             "RAYMON", &
             "RAYFORD", &
             "CRISTOBAL", &
             "AMBROSE", &
             "TITUS", &
             "HYMAN", &
             "FELTON", &
             "EZEQUIEL", &
             "ERASMO", &
             "STANTON", &
             "LONNY", &
             "LEN", &
             "IKE", &
             "MILAN", &
             "LINO", &
             "JAROD", &
             "HERB", &
             "ANDREAS", &
             "WALTON", &
             "RHETT", &
             "PALMER", &
             "DOUGLASS", &
             "CORDELL", &
             "OSWALDO", &
             "ELLSWORTH", &
             "VIRGILIO", &
             "TONEY", &
             "NATHANAEL", &
             "DEL", &
             "BENEDICT", &
             "MOSE", &
             "JOHNSON", &
             "ISREAL", &
             "GARRET", &
             "FAUSTO", &
             "ASA", &
             "ARLEN", &
             "ZACK", &
             "WARNER", &
             "MODESTO", &
             "FRANCESCO", &
             "MANUAL", &
             "GAYLORD", &
             "GASTON", &
             "FILIBERTO", &
             "DEANGELO", &
             "MICHALE", &
             "GRANVILLE", &
             "WES", &
             "MALIK", &
             "ZACKARY", &
             "TUAN", &
             "ELDRIDGE", &
             "CRISTOPHER", &
             "CORTEZ", &
             "ANTIONE", &
             "MALCOM", &
             "LONG", &
             "KOREY", &
             "JOSPEH", &
             "COLTON", &
             "WAYLON", &
             "VON", &
             "HOSEA", &
             "SHAD", &
             "SANTO", &
             "RUDOLF", &
             "ROLF", &
             "REY", &
             "RENALDO", &
             "MARCELLUS", &
             "LUCIUS", &
             "KRISTOFER", &
             "BOYCE", &
             "BENTON", &
             "HAYDEN", &
             "HARLAND", &
             "ARNOLDO", &
             "RUEBEN", &
             "LEANDRO", &
             "KRAIG", &
             "JERRELL", &
             "JEROMY", &
             "HOBERT", &
             "CEDRICK", &
             "ARLIE", &
             "WINFORD", &
             "WALLY", &
             "LUIGI", &
             "KENETH", &
             "JACINTO", &
             "GRAIG", &
             "FRANKLYN", &
             "EDMUNDO", &
             "SID", &
             "PORTER", &
             "LEIF", &
             "JERAMY", &
             "BUCK", &
             "WILLIAN", &
             "VINCENZO", &
             "SHON", &
             "LYNWOOD", &
             "JERE", &
             "HAI", &
             "ELDEN", &
             "DORSEY", &
             "DARELL", &
             "BRODERICK", &
             "ALONSO"]
    end subroutine get_euler_data_0022

    !> Data required for problem 0042.
    pure subroutine get_euler_data_0042(euler_data)
        character(len=:), allocatable, intent(out) :: euler_data(:)

        euler_data = &
            [character(len=50) :: &
             "A", &
             "ABILITY", &
             "ABLE", &
             "ABOUT", &
             "ABOVE", &
             "ABSENCE", &
             "ABSOLUTELY", &
             "ACADEMIC", &
             "ACCEPT", &
             "ACCESS", &
             "ACCIDENT", &
             "ACCOMPANY", &
             "ACCORDING", &
             "ACCOUNT", &
             "ACHIEVE", &
             "ACHIEVEMENT", &
             "ACID", &
             "ACQUIRE", &
             "ACROSS", &
             "ACT", &
             "ACTION", &
             "ACTIVE", &
             "ACTIVITY", &
             "ACTUAL", &
             "ACTUALLY", &
             "ADD", &
             "ADDITION", &
             "ADDITIONAL", &
             "ADDRESS", &
             "ADMINISTRATION", &
             "ADMIT", &
             "ADOPT", &
             "ADULT", &
             "ADVANCE", &
             "ADVANTAGE", &
             "ADVICE", &
             "ADVISE", &
             "AFFAIR", &
             "AFFECT", &
             "AFFORD", &
             "AFRAID", &
             "AFTER", &
             "AFTERNOON", &
             "AFTERWARDS", &
             "AGAIN", &
             "AGAINST", &
             "AGE", &
             "AGENCY", &
             "AGENT", &
             "AGO", &
             "AGREE", &
             "AGREEMENT", &
             "AHEAD", &
             "AID", &
             "AIM", &
             "AIR", &
             "AIRCRAFT", &
             "ALL", &
             "ALLOW", &
             "ALMOST", &
             "ALONE", &
             "ALONG", &
             "ALREADY", &
             "ALRIGHT", &
             "ALSO", &
             "ALTERNATIVE", &
             "ALTHOUGH", &
             "ALWAYS", &
             "AMONG", &
             "AMONGST", &
             "AMOUNT", &
             "AN", &
             "ANALYSIS", &
             "ANCIENT", &
             "AND", &
             "ANIMAL", &
             "ANNOUNCE", &
             "ANNUAL", &
             "ANOTHER", &
             "ANSWER", &
             "ANY", &
             "ANYBODY", &
             "ANYONE", &
             "ANYTHING", &
             "ANYWAY", &
             "APART", &
             "APPARENT", &
             "APPARENTLY", &
             "APPEAL", &
             "APPEAR", &
             "APPEARANCE", &
             "APPLICATION", &
             "APPLY", &
             "APPOINT", &
             "APPOINTMENT", &
             "APPROACH", &
             "APPROPRIATE", &
             "APPROVE", &
             "AREA", &
             "ARGUE", &
             "ARGUMENT", &
             "ARISE", &
             "ARM", &
             "ARMY", &
             "AROUND", &
             "ARRANGE", &
             "ARRANGEMENT", &
             "ARRIVE", &
             "ART", &
             "ARTICLE", &
             "ARTIST", &
             "AS", &
             "ASK", &
             "ASPECT", &
             "ASSEMBLY", &
             "ASSESS", &
             "ASSESSMENT", &
             "ASSET", &
             "ASSOCIATE", &
             "ASSOCIATION", &
             "ASSUME", &
             "ASSUMPTION", &
             "AT", &
             "ATMOSPHERE", &
             "ATTACH", &
             "ATTACK", &
             "ATTEMPT", &
             "ATTEND", &
             "ATTENTION", &
             "ATTITUDE", &
             "ATTRACT", &
             "ATTRACTIVE", &
             "AUDIENCE", &
             "AUTHOR", &
             "AUTHORITY", &
             "AVAILABLE", &
             "AVERAGE", &
             "AVOID", &
             "AWARD", &
             "AWARE", &
             "AWAY", &
             "AYE", &
             "BABY", &
             "BACK", &
             "BACKGROUND", &
             "BAD", &
             "BAG", &
             "BALANCE", &
             "BALL", &
             "BAND", &
             "BANK", &
             "BAR", &
             "BASE", &
             "BASIC", &
             "BASIS", &
             "BATTLE", &
             "BE", &
             "BEAR", &
             "BEAT", &
             "BEAUTIFUL", &
             "BECAUSE", &
             "BECOME", &
             "BED", &
             "BEDROOM", &
             "BEFORE", &
             "BEGIN", &
             "BEGINNING", &
             "BEHAVIOUR", &
             "BEHIND", &
             "BELIEF", &
             "BELIEVE", &
             "BELONG", &
             "BELOW", &
             "BENEATH", &
             "BENEFIT", &
             "BESIDE", &
             "BEST", &
             "BETTER", &
             "BETWEEN", &
             "BEYOND", &
             "BIG", &
             "BILL", &
             "BIND", &
             "BIRD", &
             "BIRTH", &
             "BIT", &
             "BLACK", &
             "BLOCK", &
             "BLOOD", &
             "BLOODY", &
             "BLOW", &
             "BLUE", &
             "BOARD", &
             "BOAT", &
             "BODY", &
             "BONE", &
             "BOOK", &
             "BORDER", &
             "BOTH", &
             "BOTTLE", &
             "BOTTOM", &
             "BOX", &
             "BOY", &
             "BRAIN", &
             "BRANCH", &
             "BREAK", &
             "BREATH", &
             "BRIDGE", &
             "BRIEF", &
             "BRIGHT", &
             "BRING", &
             "BROAD", &
             "BROTHER", &
             "BUDGET", &
             "BUILD", &
             "BUILDING", &
             "BURN", &
             "BUS", &
             "BUSINESS", &
             "BUSY", &
             "BUT", &
             "BUY", &
             "BY", &
             "CABINET", &
             "CALL", &
             "CAMPAIGN", &
             "CAN", &
             "CANDIDATE", &
             "CAPABLE", &
             "CAPACITY", &
             "CAPITAL", &
             "CAR", &
             "CARD", &
             "CARE", &
             "CAREER", &
             "CAREFUL", &
             "CAREFULLY", &
             "CARRY", &
             "CASE", &
             "CASH", &
             "CAT", &
             "CATCH", &
             "CATEGORY", &
             "CAUSE", &
             "CELL", &
             "CENTRAL", &
             "CENTRE", &
             "CENTURY", &
             "CERTAIN", &
             "CERTAINLY", &
             "CHAIN", &
             "CHAIR", &
             "CHAIRMAN", &
             "CHALLENGE", &
             "CHANCE", &
             "CHANGE", &
             "CHANNEL", &
             "CHAPTER", &
             "CHARACTER", &
             "CHARACTERISTIC", &
             "CHARGE", &
             "CHEAP", &
             "CHECK", &
             "CHEMICAL", &
             "CHIEF", &
             "CHILD", &
             "CHOICE", &
             "CHOOSE", &
             "CHURCH", &
             "CIRCLE", &
             "CIRCUMSTANCE", &
             "CITIZEN", &
             "CITY", &
             "CIVIL", &
             "CLAIM", &
             "CLASS", &
             "CLEAN", &
             "CLEAR", &
             "CLEARLY", &
             "CLIENT", &
             "CLIMB", &
             "CLOSE", &
             "CLOSELY", &
             "CLOTHES", &
             "CLUB", &
             "COAL", &
             "CODE", &
             "COFFEE", &
             "COLD", &
             "COLLEAGUE", &
             "COLLECT", &
             "COLLECTION", &
             "COLLEGE", &
             "COLOUR", &
             "COMBINATION", &
             "COMBINE", &
             "COME", &
             "COMMENT", &
             "COMMERCIAL", &
             "COMMISSION", &
             "COMMIT", &
             "COMMITMENT", &
             "COMMITTEE", &
             "COMMON", &
             "COMMUNICATION", &
             "COMMUNITY", &
             "COMPANY", &
             "COMPARE", &
             "COMPARISON", &
             "COMPETITION", &
             "COMPLETE", &
             "COMPLETELY", &
             "COMPLEX", &
             "COMPONENT", &
             "COMPUTER", &
             "CONCENTRATE", &
             "CONCENTRATION", &
             "CONCEPT", &
             "CONCERN", &
             "CONCERNED", &
             "CONCLUDE", &
             "CONCLUSION", &
             "CONDITION", &
             "CONDUCT", &
             "CONFERENCE", &
             "CONFIDENCE", &
             "CONFIRM", &
             "CONFLICT", &
             "CONGRESS", &
             "CONNECT", &
             "CONNECTION", &
             "CONSEQUENCE", &
             "CONSERVATIVE", &
             "CONSIDER", &
             "CONSIDERABLE", &
             "CONSIDERATION", &
             "CONSIST", &
             "CONSTANT", &
             "CONSTRUCTION", &
             "CONSUMER", &
             "CONTACT", &
             "CONTAIN", &
             "CONTENT", &
             "CONTEXT", &
             "CONTINUE", &
             "CONTRACT", &
             "CONTRAST", &
             "CONTRIBUTE", &
             "CONTRIBUTION", &
             "CONTROL", &
             "CONVENTION", &
             "CONVERSATION", &
             "COPY", &
             "CORNER", &
             "CORPORATE", &
             "CORRECT", &
             "COS", &
             "COST", &
             "COULD", &
             "COUNCIL", &
             "COUNT", &
             "COUNTRY", &
             "COUNTY", &
             "COUPLE", &
             "COURSE", &
             "COURT", &
             "COVER", &
             "CREATE", &
             "CREATION", &
             "CREDIT", &
             "CRIME", &
             "CRIMINAL", &
             "CRISIS", &
             "CRITERION", &
             "CRITICAL", &
             "CRITICISM", &
             "CROSS", &
             "CROWD", &
             "CRY", &
             "CULTURAL", &
             "CULTURE", &
             "CUP", &
             "CURRENT", &
             "CURRENTLY", &
             "CURRICULUM", &
             "CUSTOMER", &
             "CUT", &
             "DAMAGE", &
             "DANGER", &
             "DANGEROUS", &
             "DARK", &
             "DATA", &
             "DATE", &
             "DAUGHTER", &
             "DAY", &
             "DEAD", &
             "DEAL", &
             "DEATH", &
             "DEBATE", &
             "DEBT", &
             "DECADE", &
             "DECIDE", &
             "DECISION", &
             "DECLARE", &
             "DEEP", &
             "DEFENCE", &
             "DEFENDANT", &
             "DEFINE", &
             "DEFINITION", &
             "DEGREE", &
             "DELIVER", &
             "DEMAND", &
             "DEMOCRATIC", &
             "DEMONSTRATE", &
             "DENY", &
             "DEPARTMENT", &
             "DEPEND", &
             "DEPUTY", &
             "DERIVE", &
             "DESCRIBE", &
             "DESCRIPTION", &
             "DESIGN", &
             "DESIRE", &
             "DESK", &
             "DESPITE", &
             "DESTROY", &
             "DETAIL", &
             "DETAILED", &
             "DETERMINE", &
             "DEVELOP", &
             "DEVELOPMENT", &
             "DEVICE", &
             "DIE", &
             "DIFFERENCE", &
             "DIFFERENT", &
             "DIFFICULT", &
             "DIFFICULTY", &
             "DINNER", &
             "DIRECT", &
             "DIRECTION", &
             "DIRECTLY", &
             "DIRECTOR", &
             "DISAPPEAR", &
             "DISCIPLINE", &
             "DISCOVER", &
             "DISCUSS", &
             "DISCUSSION", &
             "DISEASE", &
             "DISPLAY", &
             "DISTANCE", &
             "DISTINCTION", &
             "DISTRIBUTION", &
             "DISTRICT", &
             "DIVIDE", &
             "DIVISION", &
             "DO", &
             "DOCTOR", &
             "DOCUMENT", &
             "DOG", &
             "DOMESTIC", &
             "DOOR", &
             "DOUBLE", &
             "DOUBT", &
             "DOWN", &
             "DRAW", &
             "DRAWING", &
             "DREAM", &
             "DRESS", &
             "DRINK", &
             "DRIVE", &
             "DRIVER", &
             "DROP", &
             "DRUG", &
             "DRY", &
             "DUE", &
             "DURING", &
             "DUTY", &
             "EACH", &
             "EAR", &
             "EARLY", &
             "EARN", &
             "EARTH", &
             "EASILY", &
             "EAST", &
             "EASY", &
             "EAT", &
             "ECONOMIC", &
             "ECONOMY", &
             "EDGE", &
             "EDITOR", &
             "EDUCATION", &
             "EDUCATIONAL", &
             "EFFECT", &
             "EFFECTIVE", &
             "EFFECTIVELY", &
             "EFFORT", &
             "EGG", &
             "EITHER", &
             "ELDERLY", &
             "ELECTION", &
             "ELEMENT", &
             "ELSE", &
             "ELSEWHERE", &
             "EMERGE", &
             "EMPHASIS", &
             "EMPLOY", &
             "EMPLOYEE", &
             "EMPLOYER", &
             "EMPLOYMENT", &
             "EMPTY", &
             "ENABLE", &
             "ENCOURAGE", &
             "END", &
             "ENEMY", &
             "ENERGY", &
             "ENGINE", &
             "ENGINEERING", &
             "ENJOY", &
             "ENOUGH", &
             "ENSURE", &
             "ENTER", &
             "ENTERPRISE", &
             "ENTIRE", &
             "ENTIRELY", &
             "ENTITLE", &
             "ENTRY", &
             "ENVIRONMENT", &
             "ENVIRONMENTAL", &
             "EQUAL", &
             "EQUALLY", &
             "EQUIPMENT", &
             "ERROR", &
             "ESCAPE", &
             "ESPECIALLY", &
             "ESSENTIAL", &
             "ESTABLISH", &
             "ESTABLISHMENT", &
             "ESTATE", &
             "ESTIMATE", &
             "EVEN", &
             "EVENING", &
             "EVENT", &
             "EVENTUALLY", &
             "EVER", &
             "EVERY", &
             "EVERYBODY", &
             "EVERYONE", &
             "EVERYTHING", &
             "EVIDENCE", &
             "EXACTLY", &
             "EXAMINATION", &
             "EXAMINE", &
             "EXAMPLE", &
             "EXCELLENT", &
             "EXCEPT", &
             "EXCHANGE", &
             "EXECUTIVE", &
             "EXERCISE", &
             "EXHIBITION", &
             "EXIST", &
             "EXISTENCE", &
             "EXISTING", &
             "EXPECT", &
             "EXPECTATION", &
             "EXPENDITURE", &
             "EXPENSE", &
             "EXPENSIVE", &
             "EXPERIENCE", &
             "EXPERIMENT", &
             "EXPERT", &
             "EXPLAIN", &
             "EXPLANATION", &
             "EXPLORE", &
             "EXPRESS", &
             "EXPRESSION", &
             "EXTEND", &
             "EXTENT", &
             "EXTERNAL", &
             "EXTRA", &
             "EXTREMELY", &
             "EYE", &
             "FACE", &
             "FACILITY", &
             "FACT", &
             "FACTOR", &
             "FACTORY", &
             "FAIL", &
             "FAILURE", &
             "FAIR", &
             "FAIRLY", &
             "FAITH", &
             "FALL", &
             "FAMILIAR", &
             "FAMILY", &
             "FAMOUS", &
             "FAR", &
             "FARM", &
             "FARMER", &
             "FASHION", &
             "FAST", &
             "FATHER", &
             "FAVOUR", &
             "FEAR", &
             "FEATURE", &
             "FEE", &
             "FEEL", &
             "FEELING", &
             "FEMALE", &
             "FEW", &
             "FIELD", &
             "FIGHT", &
             "FIGURE", &
             "FILE", &
             "FILL", &
             "FILM", &
             "FINAL", &
             "FINALLY", &
             "FINANCE", &
             "FINANCIAL", &
             "FIND", &
             "FINDING", &
             "FINE", &
             "FINGER", &
             "FINISH", &
             "FIRE", &
             "FIRM", &
             "FIRST", &
             "FISH", &
             "FIT", &
             "FIX", &
             "FLAT", &
             "FLIGHT", &
             "FLOOR", &
             "FLOW", &
             "FLOWER", &
             "FLY", &
             "FOCUS", &
             "FOLLOW", &
             "FOLLOWING", &
             "FOOD", &
             "FOOT", &
             "FOOTBALL", &
             "FOR", &
             "FORCE", &
             "FOREIGN", &
             "FOREST", &
             "FORGET", &
             "FORM", &
             "FORMAL", &
             "FORMER", &
             "FORWARD", &
             "FOUNDATION", &
             "FREE", &
             "FREEDOM", &
             "FREQUENTLY", &
             "FRESH", &
             "FRIEND", &
             "FROM", &
             "FRONT", &
             "FRUIT", &
             "FUEL", &
             "FULL", &
             "FULLY", &
             "FUNCTION", &
             "FUND", &
             "FUNNY", &
             "FURTHER", &
             "FUTURE", &
             "GAIN", &
             "GAME", &
             "GARDEN", &
             "GAS", &
             "GATE", &
             "GATHER", &
             "GENERAL", &
             "GENERALLY", &
             "GENERATE", &
             "GENERATION", &
             "GENTLEMAN", &
             "GET", &
             "GIRL", &
             "GIVE", &
             "GLASS", &
             "GO", &
             "GOAL", &
             "GOD", &
             "GOLD", &
             "GOOD", &
             "GOVERNMENT", &
             "GRANT", &
             "GREAT", &
             "GREEN", &
             "GREY", &
             "GROUND", &
             "GROUP", &
             "GROW", &
             "GROWING", &
             "GROWTH", &
             "GUEST", &
             "GUIDE", &
             "GUN", &
             "HAIR", &
             "HALF", &
             "HALL", &
             "HAND", &
             "HANDLE", &
             "HANG", &
             "HAPPEN", &
             "HAPPY", &
             "HARD", &
             "HARDLY", &
             "HATE", &
             "HAVE", &
             "HE", &
             "HEAD", &
             "HEALTH", &
             "HEAR", &
             "HEART", &
             "HEAT", &
             "HEAVY", &
             "HELL", &
             "HELP", &
             "HENCE", &
             "HER", &
             "HERE", &
             "HERSELF", &
             "HIDE", &
             "HIGH", &
             "HIGHLY", &
             "HILL", &
             "HIM", &
             "HIMSELF", &
             "HIS", &
             "HISTORICAL", &
             "HISTORY", &
             "HIT", &
             "HOLD", &
             "HOLE", &
             "HOLIDAY", &
             "HOME", &
             "HOPE", &
             "HORSE", &
             "HOSPITAL", &
             "HOT", &
             "HOTEL", &
             "HOUR", &
             "HOUSE", &
             "HOUSEHOLD", &
             "HOUSING", &
             "HOW", &
             "HOWEVER", &
             "HUGE", &
             "HUMAN", &
             "HURT", &
             "HUSBAND", &
             "I", &
             "IDEA", &
             "IDENTIFY", &
             "IF", &
             "IGNORE", &
             "ILLUSTRATE", &
             "IMAGE", &
             "IMAGINE", &
             "IMMEDIATE", &
             "IMMEDIATELY", &
             "IMPACT", &
             "IMPLICATION", &
             "IMPLY", &
             "IMPORTANCE", &
             "IMPORTANT", &
             "IMPOSE", &
             "IMPOSSIBLE", &
             "IMPRESSION", &
             "IMPROVE", &
             "IMPROVEMENT", &
             "IN", &
             "INCIDENT", &
             "INCLUDE", &
             "INCLUDING", &
             "INCOME", &
             "INCREASE", &
             "INCREASED", &
             "INCREASINGLY", &
             "INDEED", &
             "INDEPENDENT", &
             "INDEX", &
             "INDICATE", &
             "INDIVIDUAL", &
             "INDUSTRIAL", &
             "INDUSTRY", &
             "INFLUENCE", &
             "INFORM", &
             "INFORMATION", &
             "INITIAL", &
             "INITIATIVE", &
             "INJURY", &
             "INSIDE", &
             "INSIST", &
             "INSTANCE", &
             "INSTEAD", &
             "INSTITUTE", &
             "INSTITUTION", &
             "INSTRUCTION", &
             "INSTRUMENT", &
             "INSURANCE", &
             "INTEND", &
             "INTENTION", &
             "INTEREST", &
             "INTERESTED", &
             "INTERESTING", &
             "INTERNAL", &
             "INTERNATIONAL", &
             "INTERPRETATION", &
             "INTERVIEW", &
             "INTO", &
             "INTRODUCE", &
             "INTRODUCTION", &
             "INVESTIGATE", &
             "INVESTIGATION", &
             "INVESTMENT", &
             "INVITE", &
             "INVOLVE", &
             "IRON", &
             "IS", &
             "ISLAND", &
             "ISSUE", &
             "IT", &
             "ITEM", &
             "ITS", &
             "ITSELF", &
             "JOB", &
             "JOIN", &
             "JOINT", &
             "JOURNEY", &
             "JUDGE", &
             "JUMP", &
             "JUST", &
             "JUSTICE", &
             "KEEP", &
             "KEY", &
             "KID", &
             "KILL", &
             "KIND", &
             "KING", &
             "KITCHEN", &
             "KNEE", &
             "KNOW", &
             "KNOWLEDGE", &
             "LABOUR", &
             "LACK", &
             "LADY", &
             "LAND", &
             "LANGUAGE", &
             "LARGE", &
             "LARGELY", &
             "LAST", &
             "LATE", &
             "LATER", &
             "LATTER", &
             "LAUGH", &
             "LAUNCH", &
             "LAW", &
             "LAWYER", &
             "LAY", &
             "LEAD", &
             "LEADER", &
             "LEADERSHIP", &
             "LEADING", &
             "LEAF", &
             "LEAGUE", &
             "LEAN", &
             "LEARN", &
             "LEAST", &
             "LEAVE", &
             "LEFT", &
             "LEG", &
             "LEGAL", &
             "LEGISLATION", &
             "LENGTH", &
             "LESS", &
             "LET", &
             "LETTER", &
             "LEVEL", &
             "LIABILITY", &
             "LIBERAL", &
             "LIBRARY", &
             "LIE", &
             "LIFE", &
             "LIFT", &
             "LIGHT", &
             "LIKE", &
             "LIKELY", &
             "LIMIT", &
             "LIMITED", &
             "LINE", &
             "LINK", &
             "LIP", &
             "LIST", &
             "LISTEN", &
             "LITERATURE", &
             "LITTLE", &
             "LIVE", &
             "LIVING", &
             "LOAN", &
             "LOCAL", &
             "LOCATION", &
             "LONG", &
             "LOOK", &
             "LORD", &
             "LOSE", &
             "LOSS", &
             "LOT", &
             "LOVE", &
             "LOVELY", &
             "LOW", &
             "LUNCH", &
             "MACHINE", &
             "MAGAZINE", &
             "MAIN", &
             "MAINLY", &
             "MAINTAIN", &
             "MAJOR", &
             "MAJORITY", &
             "MAKE", &
             "MALE", &
             "MAN", &
             "MANAGE", &
             "MANAGEMENT", &
             "MANAGER", &
             "MANNER", &
             "MANY", &
             "MAP", &
             "MARK", &
             "MARKET", &
             "MARRIAGE", &
             "MARRIED", &
             "MARRY", &
             "MASS", &
             "MASTER", &
             "MATCH", &
             "MATERIAL", &
             "MATTER", &
             "MAY", &
             "MAYBE", &
             "ME", &
             "MEAL", &
             "MEAN", &
             "MEANING", &
             "MEANS", &
             "MEANWHILE", &
             "MEASURE", &
             "MECHANISM", &
             "MEDIA", &
             "MEDICAL", &
             "MEET", &
             "MEETING", &
             "MEMBER", &
             "MEMBERSHIP", &
             "MEMORY", &
             "MENTAL", &
             "MENTION", &
             "MERELY", &
             "MESSAGE", &
             "METAL", &
             "METHOD", &
             "MIDDLE", &
             "MIGHT", &
             "MILE", &
             "MILITARY", &
             "MILK", &
             "MIND", &
             "MINE", &
             "MINISTER", &
             "MINISTRY", &
             "MINUTE", &
             "MISS", &
             "MISTAKE", &
             "MODEL", &
             "MODERN", &
             "MODULE", &
             "MOMENT", &
             "MONEY", &
             "MONTH", &
             "MORE", &
             "MORNING", &
             "MOST", &
             "MOTHER", &
             "MOTION", &
             "MOTOR", &
             "MOUNTAIN", &
             "MOUTH", &
             "MOVE", &
             "MOVEMENT", &
             "MUCH", &
             "MURDER", &
             "MUSEUM", &
             "MUSIC", &
             "MUST", &
             "MY", &
             "MYSELF", &
             "NAME", &
             "NARROW", &
             "NATION", &
             "NATIONAL", &
             "NATURAL", &
             "NATURE", &
             "NEAR", &
             "NEARLY", &
             "NECESSARILY", &
             "NECESSARY", &
             "NECK", &
             "NEED", &
             "NEGOTIATION", &
             "NEIGHBOUR", &
             "NEITHER", &
             "NETWORK", &
             "NEVER", &
             "NEVERTHELESS", &
             "NEW", &
             "NEWS", &
             "NEWSPAPER", &
             "NEXT", &
             "NICE", &
             "NIGHT", &
             "NO", &
             "NOBODY", &
             "NOD", &
             "NOISE", &
             "NONE", &
             "NOR", &
             "NORMAL", &
             "NORMALLY", &
             "NORTH", &
             "NORTHERN", &
             "NOSE", &
             "NOT", &
             "NOTE", &
             "NOTHING", &
             "NOTICE", &
             "NOTION", &
             "NOW", &
             "NUCLEAR", &
             "NUMBER", &
             "NURSE", &
             "OBJECT", &
             "OBJECTIVE", &
             "OBSERVATION", &
             "OBSERVE", &
             "OBTAIN", &
             "OBVIOUS", &
             "OBVIOUSLY", &
             "OCCASION", &
             "OCCUR", &
             "ODD", &
             "OF", &
             "OFF", &
             "OFFENCE", &
             "OFFER", &
             "OFFICE", &
             "OFFICER", &
             "OFFICIAL", &
             "OFTEN", &
             "OIL", &
             "OKAY", &
             "OLD", &
             "ON", &
             "ONCE", &
             "ONE", &
             "ONLY", &
             "ONTO", &
             "OPEN", &
             "OPERATE", &
             "OPERATION", &
             "OPINION", &
             "OPPORTUNITY", &
             "OPPOSITION", &
             "OPTION", &
             "OR", &
             "ORDER", &
             "ORDINARY", &
             "ORGANISATION", &
             "ORGANISE", &
             "ORGANIZATION", &
             "ORIGIN", &
             "ORIGINAL", &
             "OTHER", &
             "OTHERWISE", &
             "OUGHT", &
             "OUR", &
             "OURSELVES", &
             "OUT", &
             "OUTCOME", &
             "OUTPUT", &
             "OUTSIDE", &
             "OVER", &
             "OVERALL", &
             "OWN", &
             "OWNER", &
             "PACKAGE", &
             "PAGE", &
             "PAIN", &
             "PAINT", &
             "PAINTING", &
             "PAIR", &
             "PANEL", &
             "PAPER", &
             "PARENT", &
             "PARK", &
             "PARLIAMENT", &
             "PART", &
             "PARTICULAR", &
             "PARTICULARLY", &
             "PARTLY", &
             "PARTNER", &
             "PARTY", &
             "PASS", &
             "PASSAGE", &
             "PAST", &
             "PATH", &
             "PATIENT", &
             "PATTERN", &
             "PAY", &
             "PAYMENT", &
             "PEACE", &
             "PENSION", &
             "PEOPLE", &
             "PER", &
             "PERCENT", &
             "PERFECT", &
             "PERFORM", &
             "PERFORMANCE", &
             "PERHAPS", &
             "PERIOD", &
             "PERMANENT", &
             "PERSON", &
             "PERSONAL", &
             "PERSUADE", &
             "PHASE", &
             "PHONE", &
             "PHOTOGRAPH", &
             "PHYSICAL", &
             "PICK", &
             "PICTURE", &
             "PIECE", &
             "PLACE", &
             "PLAN", &
             "PLANNING", &
             "PLANT", &
             "PLASTIC", &
             "PLATE", &
             "PLAY", &
             "PLAYER", &
             "PLEASE", &
             "PLEASURE", &
             "PLENTY", &
             "PLUS", &
             "POCKET", &
             "POINT", &
             "POLICE", &
             "POLICY", &
             "POLITICAL", &
             "POLITICS", &
             "POOL", &
             "POOR", &
             "POPULAR", &
             "POPULATION", &
             "POSITION", &
             "POSITIVE", &
             "POSSIBILITY", &
             "POSSIBLE", &
             "POSSIBLY", &
             "POST", &
             "POTENTIAL", &
             "POUND", &
             "POWER", &
             "POWERFUL", &
             "PRACTICAL", &
             "PRACTICE", &
             "PREFER", &
             "PREPARE", &
             "PRESENCE", &
             "PRESENT", &
             "PRESIDENT", &
             "PRESS", &
             "PRESSURE", &
             "PRETTY", &
             "PREVENT", &
             "PREVIOUS", &
             "PREVIOUSLY", &
             "PRICE", &
             "PRIMARY", &
             "PRIME", &
             "PRINCIPLE", &
             "PRIORITY", &
             "PRISON", &
             "PRISONER", &
             "PRIVATE", &
             "PROBABLY", &
             "PROBLEM", &
             "PROCEDURE", &
             "PROCESS", &
             "PRODUCE", &
             "PRODUCT", &
             "PRODUCTION", &
             "PROFESSIONAL", &
             "PROFIT", &
             "PROGRAM", &
             "PROGRAMME", &
             "PROGRESS", &
             "PROJECT", &
             "PROMISE", &
             "PROMOTE", &
             "PROPER", &
             "PROPERLY", &
             "PROPERTY", &
             "PROPORTION", &
             "PROPOSE", &
             "PROPOSAL", &
             "PROSPECT", &
             "PROTECT", &
             "PROTECTION", &
             "PROVE", &
             "PROVIDE", &
             "PROVIDED", &
             "PROVISION", &
             "PUB", &
             "PUBLIC", &
             "PUBLICATION", &
             "PUBLISH", &
             "PULL", &
             "PUPIL", &
             "PURPOSE", &
             "PUSH", &
             "PUT", &
             "QUALITY", &
             "QUARTER", &
             "QUESTION", &
             "QUICK", &
             "QUICKLY", &
             "QUIET", &
             "QUITE", &
             "RACE", &
             "RADIO", &
             "RAILWAY", &
             "RAIN", &
             "RAISE", &
             "RANGE", &
             "RAPIDLY", &
             "RARE", &
             "RATE", &
             "RATHER", &
             "REACH", &
             "REACTION", &
             "READ", &
             "READER", &
             "READING", &
             "READY", &
             "REAL", &
             "REALISE", &
             "REALITY", &
             "REALIZE", &
             "REALLY", &
             "REASON", &
             "REASONABLE", &
             "RECALL", &
             "RECEIVE", &
             "RECENT", &
             "RECENTLY", &
             "RECOGNISE", &
             "RECOGNITION", &
             "RECOGNIZE", &
             "RECOMMEND", &
             "RECORD", &
             "RECOVER", &
             "RED", &
             "REDUCE", &
             "REDUCTION", &
             "REFER", &
             "REFERENCE", &
             "REFLECT", &
             "REFORM", &
             "REFUSE", &
             "REGARD", &
             "REGION", &
             "REGIONAL", &
             "REGULAR", &
             "REGULATION", &
             "REJECT", &
             "RELATE", &
             "RELATION", &
             "RELATIONSHIP", &
             "RELATIVE", &
             "RELATIVELY", &
             "RELEASE", &
             "RELEVANT", &
             "RELIEF", &
             "RELIGION", &
             "RELIGIOUS", &
             "RELY", &
             "REMAIN", &
             "REMEMBER", &
             "REMIND", &
             "REMOVE", &
             "REPEAT", &
             "REPLACE", &
             "REPLY", &
             "REPORT", &
             "REPRESENT", &
             "REPRESENTATION", &
             "REPRESENTATIVE", &
             "REQUEST", &
             "REQUIRE", &
             "REQUIREMENT", &
             "RESEARCH", &
             "RESOURCE", &
             "RESPECT", &
             "RESPOND", &
             "RESPONSE", &
             "RESPONSIBILITY", &
             "RESPONSIBLE", &
             "REST", &
             "RESTAURANT", &
             "RESULT", &
             "RETAIN", &
             "RETURN", &
             "REVEAL", &
             "REVENUE", &
             "REVIEW", &
             "REVOLUTION", &
             "RICH", &
             "RIDE", &
             "RIGHT", &
             "RING", &
             "RISE", &
             "RISK", &
             "RIVER", &
             "ROAD", &
             "ROCK", &
             "ROLE", &
             "ROLL", &
             "ROOF", &
             "ROOM", &
             "ROUND", &
             "ROUTE", &
             "ROW", &
             "ROYAL", &
             "RULE", &
             "RUN", &
             "RURAL", &
             "SAFE", &
             "SAFETY", &
             "SALE", &
             "SAME", &
             "SAMPLE", &
             "SATISFY", &
             "SAVE", &
             "SAY", &
             "SCALE", &
             "SCENE", &
             "SCHEME", &
             "SCHOOL", &
             "SCIENCE", &
             "SCIENTIFIC", &
             "SCIENTIST", &
             "SCORE", &
             "SCREEN", &
             "SEA", &
             "SEARCH", &
             "SEASON", &
             "SEAT", &
             "SECOND", &
             "SECONDARY", &
             "SECRETARY", &
             "SECTION", &
             "SECTOR", &
             "SECURE", &
             "SECURITY", &
             "SEE", &
             "SEEK", &
             "SEEM", &
             "SELECT", &
             "SELECTION", &
             "SELL", &
             "SEND", &
             "SENIOR", &
             "SENSE", &
             "SENTENCE", &
             "SEPARATE", &
             "SEQUENCE", &
             "SERIES", &
             "SERIOUS", &
             "SERIOUSLY", &
             "SERVANT", &
             "SERVE", &
             "SERVICE", &
             "SESSION", &
             "SET", &
             "SETTLE", &
             "SETTLEMENT", &
             "SEVERAL", &
             "SEVERE", &
             "SEX", &
             "SEXUAL", &
             "SHAKE", &
             "SHALL", &
             "SHAPE", &
             "SHARE", &
             "SHE", &
             "SHEET", &
             "SHIP", &
             "SHOE", &
             "SHOOT", &
             "SHOP", &
             "SHORT", &
             "SHOT", &
             "SHOULD", &
             "SHOULDER", &
             "SHOUT", &
             "SHOW", &
             "SHUT", &
             "SIDE", &
             "SIGHT", &
             "SIGN", &
             "SIGNAL", &
             "SIGNIFICANCE", &
             "SIGNIFICANT", &
             "SILENCE", &
             "SIMILAR", &
             "SIMPLE", &
             "SIMPLY", &
             "SINCE", &
             "SING", &
             "SINGLE", &
             "SIR", &
             "SISTER", &
             "SIT", &
             "SITE", &
             "SITUATION", &
             "SIZE", &
             "SKILL", &
             "SKIN", &
             "SKY", &
             "SLEEP", &
             "SLIGHTLY", &
             "SLIP", &
             "SLOW", &
             "SLOWLY", &
             "SMALL", &
             "SMILE", &
             "SO", &
             "SOCIAL", &
             "SOCIETY", &
             "SOFT", &
             "SOFTWARE", &
             "SOIL", &
             "SOLDIER", &
             "SOLICITOR", &
             "SOLUTION", &
             "SOME", &
             "SOMEBODY", &
             "SOMEONE", &
             "SOMETHING", &
             "SOMETIMES", &
             "SOMEWHAT", &
             "SOMEWHERE", &
             "SON", &
             "SONG", &
             "SOON", &
             "SORRY", &
             "SORT", &
             "SOUND", &
             "SOURCE", &
             "SOUTH", &
             "SOUTHERN", &
             "SPACE", &
             "SPEAK", &
             "SPEAKER", &
             "SPECIAL", &
             "SPECIES", &
             "SPECIFIC", &
             "SPEECH", &
             "SPEED", &
             "SPEND", &
             "SPIRIT", &
             "SPORT", &
             "SPOT", &
             "SPREAD", &
             "SPRING", &
             "STAFF", &
             "STAGE", &
             "STAND", &
             "STANDARD", &
             "STAR", &
             "START", &
             "STATE", &
             "STATEMENT", &
             "STATION", &
             "STATUS", &
             "STAY", &
             "STEAL", &
             "STEP", &
             "STICK", &
             "STILL", &
             "STOCK", &
             "STONE", &
             "STOP", &
             "STORE", &
             "STORY", &
             "STRAIGHT", &
             "STRANGE", &
             "STRATEGY", &
             "STREET", &
             "STRENGTH", &
             "STRIKE", &
             "STRONG", &
             "STRONGLY", &
             "STRUCTURE", &
             "STUDENT", &
             "STUDIO", &
             "STUDY", &
             "STUFF", &
             "STYLE", &
             "SUBJECT", &
             "SUBSTANTIAL", &
             "SUCCEED", &
             "SUCCESS", &
             "SUCCESSFUL", &
             "SUCH", &
             "SUDDENLY", &
             "SUFFER", &
             "SUFFICIENT", &
             "SUGGEST", &
             "SUGGESTION", &
             "SUITABLE", &
             "SUM", &
             "SUMMER", &
             "SUN", &
             "SUPPLY", &
             "SUPPORT", &
             "SUPPOSE", &
             "SURE", &
             "SURELY", &
             "SURFACE", &
             "SURPRISE", &
             "SURROUND", &
             "SURVEY", &
             "SURVIVE", &
             "SWITCH", &
             "SYSTEM", &
             "TABLE", &
             "TAKE", &
             "TALK", &
             "TALL", &
             "TAPE", &
             "TARGET", &
             "TASK", &
             "TAX", &
             "TEA", &
             "TEACH", &
             "TEACHER", &
             "TEACHING", &
             "TEAM", &
             "TEAR", &
             "TECHNICAL", &
             "TECHNIQUE", &
             "TECHNOLOGY", &
             "TELEPHONE", &
             "TELEVISION", &
             "TELL", &
             "TEMPERATURE", &
             "TEND", &
             "TERM", &
             "TERMS", &
             "TERRIBLE", &
             "TEST", &
             "TEXT", &
             "THAN", &
             "THANK", &
             "THANKS", &
             "THAT", &
             "THE", &
             "THEATRE", &
             "THEIR", &
             "THEM", &
             "THEME", &
             "THEMSELVES", &
             "THEN", &
             "THEORY", &
             "THERE", &
             "THEREFORE", &
             "THESE", &
             "THEY", &
             "THIN", &
             "THING", &
             "THINK", &
             "THIS", &
             "THOSE", &
             "THOUGH", &
             "THOUGHT", &
             "THREAT", &
             "THREATEN", &
             "THROUGH", &
             "THROUGHOUT", &
             "THROW", &
             "THUS", &
             "TICKET", &
             "TIME", &
             "TINY", &
             "TITLE", &
             "TO", &
             "TODAY", &
             "TOGETHER", &
             "TOMORROW", &
             "TONE", &
             "TONIGHT", &
             "TOO", &
             "TOOL", &
             "TOOTH", &
             "TOP", &
             "TOTAL", &
             "TOTALLY", &
             "TOUCH", &
             "TOUR", &
             "TOWARDS", &
             "TOWN", &
             "TRACK", &
             "TRADE", &
             "TRADITION", &
             "TRADITIONAL", &
             "TRAFFIC", &
             "TRAIN", &
             "TRAINING", &
             "TRANSFER", &
             "TRANSPORT", &
             "TRAVEL", &
             "TREAT", &
             "TREATMENT", &
             "TREATY", &
             "TREE", &
             "TREND", &
             "TRIAL", &
             "TRIP", &
             "TROOP", &
             "TROUBLE", &
             "TRUE", &
             "TRUST", &
             "TRUTH", &
             "TRY", &
             "TURN", &
             "TWICE", &
             "TYPE", &
             "TYPICAL", &
             "UNABLE", &
             "UNDER", &
             "UNDERSTAND", &
             "UNDERSTANDING", &
             "UNDERTAKE", &
             "UNEMPLOYMENT", &
             "UNFORTUNATELY", &
             "UNION", &
             "UNIT", &
             "UNITED", &
             "UNIVERSITY", &
             "UNLESS", &
             "UNLIKELY", &
             "UNTIL", &
             "UP", &
             "UPON", &
             "UPPER", &
             "URBAN", &
             "US", &
             "USE", &
             "USED", &
             "USEFUL", &
             "USER", &
             "USUAL", &
             "USUALLY", &
             "VALUE", &
             "VARIATION", &
             "VARIETY", &
             "VARIOUS", &
             "VARY", &
             "VAST", &
             "VEHICLE", &
             "VERSION", &
             "VERY", &
             "VIA", &
             "VICTIM", &
             "VICTORY", &
             "VIDEO", &
             "VIEW", &
             "VILLAGE", &
             "VIOLENCE", &
             "VISION", &
             "VISIT", &
             "VISITOR", &
             "VITAL", &
             "VOICE", &
             "VOLUME", &
             "VOTE", &
             "WAGE", &
             "WAIT", &
             "WALK", &
             "WALL", &
             "WANT", &
             "WAR", &
             "WARM", &
             "WARN", &
             "WASH", &
             "WATCH", &
             "WATER", &
             "WAVE", &
             "WAY", &
             "WE", &
             "WEAK", &
             "WEAPON", &
             "WEAR", &
             "WEATHER", &
             "WEEK", &
             "WEEKEND", &
             "WEIGHT", &
             "WELCOME", &
             "WELFARE", &
             "WELL", &
             "WEST", &
             "WESTERN", &
             "WHAT", &
             "WHATEVER", &
             "WHEN", &
             "WHERE", &
             "WHEREAS", &
             "WHETHER", &
             "WHICH", &
             "WHILE", &
             "WHILST", &
             "WHITE", &
             "WHO", &
             "WHOLE", &
             "WHOM", &
             "WHOSE", &
             "WHY", &
             "WIDE", &
             "WIDELY", &
             "WIFE", &
             "WILD", &
             "WILL", &
             "WIN", &
             "WIND", &
             "WINDOW", &
             "WINE", &
             "WING", &
             "WINNER", &
             "WINTER", &
             "WISH", &
             "WITH", &
             "WITHDRAW", &
             "WITHIN", &
             "WITHOUT", &
             "WOMAN", &
             "WONDER", &
             "WONDERFUL", &
             "WOOD", &
             "WORD", &
             "WORK", &
             "WORKER", &
             "WORKING", &
             "WORKS", &
             "WORLD", &
             "WORRY", &
             "WORTH", &
             "WOULD", &
             "WRITE", &
             "WRITER", &
             "WRITING", &
             "WRONG", &
             "YARD", &
             "YEAH", &
             "YEAR", &
             "YES", &
             "YESTERDAY", &
             "YET", &
             "YOU", &
             "YOUNG", &
             "YOUR", &
             "YOURSELF", &
             "YOUTH"]
    end subroutine get_euler_data_0042

    !> Data required for problem 0054.
    pure subroutine get_euler_data_0054(euler_data)
        character(len=:), allocatable, intent(out) :: euler_data(:)

        euler_data = &
            [character(len=30) :: &
             "8C TS KC 9H 4S 7D 2S 5D 3S AC", &
             "5C AD 5D AC 9C 7C 5H 8D TD KS", &
             "3H 7H 6S KC JS QH TD JC 2D 8S", &
             "TH 8H 5C QS TC 9H 4D JC KS JS", &
             "7C 5H KC QH JD AS KH 4C AD 4S", &
             "5H KS 9C 7D 9H 8D 3S 5D 5C AH", &
             "6H 4H 5C 3H 2H 3S QH 5S 6S AS", &
             "TD 8C 4H 7C TC KC 4C 3H 7S KS", &
             "7C 9C 6D KD 3H 4C QS QC AC KH", &
             "JC 6S 5H 2H 2D KD 9D 7C AS JS", &
             "AD QH TH 9D 8H TS 6D 3S AS AC", &
             "2H 4S 5C 5S TC KC JD 6C TS 3C", &
             "QD AS 6H JS 2C 3D 9H KC 4H 8S", &
             "KD 8S 9S 7C 2S 3S 6D 6S 4H KC", &
             "3C 8C 2D 7D 4D 9S 4S QH 4H JD", &
             "8C KC 7S TC 2D TS 8H QD AC 5C", &
             "3D KH QD 6C 6S AD AS 8H 2H QS", &
             "6S 8D 4C 8S 6C QH TC 6D 7D 9D", &
             "2S 8D 8C 4C TS 9S 9D 9C AC 3D", &
             "3C QS 2S 4H JH 3D 2D TD 8S 9H", &
             "5H QS 8S 6D 3C 8C JD AS 7H 7D", &
             "6H TD 9D AS JH 6C QC 9S KD JC", &
             "AH 8S QS 4D TH AC TS 3C 3D 5C", &
             "5S 4D JS 3D 8H 6C TS 3S AD 8C", &
             "6D 7C 5D 5H 3S 5C JC 2H 5S 3D", &
             "5H 6H 2S KS 3D 5D JD 7H JS 8H", &
             "KH 4H AS JS QS QC TC 6D 7C KS", &
             "3D QS TS 2H JS 4D AS 9S JC KD", &
             "QD 5H 4D 5D KH 7H 3D JS KD 4H", &
             "2C 9H 6H 5C 9D 6C JC 2D TH 9S", &
             "7D 6D AS QD JH 4D JS 7C QS 5C", &
             "3H KH QD AD 8C 8H 3S TH 9D 5S", &
             "AH 9S 4D 9D 8S 4H JS 3C TC 8D", &
             "2C KS 5H QD 3S TS 9H AH AD 8S", &
             "5C 7H 5D KD 9H 4D 3D 2D KS AD", &
             "KS KC 9S 6D 2C QH 9D 9H TS TC", &
             "9C 6H 5D QH 4D AD 6D QC JS KH", &
             "9S 3H 9D JD 5C 4D 9H AS TC QH", &
             "2C 6D JC 9C 3C AD 9S KH 9D 7D", &
             "KC 9C 7C JC JS KD 3H AS 3C 7D", &
             "QD KH QS 2C 3S 8S 8H 9H 9C JC", &
             "QH 8D 3C KC 4C 4H 6D AD 9H 9D", &
             "3S KS QS 7H KH 7D 5H 5D JD AD", &
             "2H 2C 6H TH TC 7D 8D 4H 8C AS", &
             "4S 2H AC QC 3S 6D TH 4D 4C KH", &
             "4D TC KS AS 7C 3C 6D 2D 9H 6C", &
             "8C TD 5D QS 2C 7H 4C 9C 3H 9H", &
             "5H JH TS 7S TD 6H AD QD 8H 8S", &
             "5S AD 9C 8C 7C 8D 5H 9D 8S 2S", &
             "4H KH KS 9S 2S KC 5S AD 4S 7D", &
             "QS 9C QD 6H JS 5D AC 8D 2S AS", &
             "KH AC JC 3S 9D 9S 3C 9C 5S JS", &
             "AD 3C 3D KS 3S 5C 9C 8C TS 4S", &
             "JH 8D 5D 6H KD QS QD 3D 6C KC", &
             "8S JD 6C 3S 8C TC QC 3C QH JS", &
             "KC JC 8H 2S 9H 9C JH 8S 8C 9S", &
             "8S 2H QH 4D QC 9D KC AS TH 3C", &
             "8S 6H TH 7C 2H 6S 3C 3H AS 7S", &
             "QH 5S JS 4H 5H TS 8H AH AC JC", &
             "9D 8H 2S 4S TC JC 3C 7H 3H 5C", &
             "3D AD 3C 3S 4C QC AS 5D TH 8C", &
             "6S 9D 4C JS KH AH TS JD 8H AD", &
             "4C 6S 9D 7S AC 4D 3D 3S TC JD", &
             "AD 7H 6H 4H JH KC TD TS 7D 6S", &
             "8H JH TC 3S 8D 8C 9S 2C 5C 4D", &
             "2C 9D KC QH TH QS JC 9C 4H TS", &
             "QS 3C QD 8H KH 4H 8D TD 8S AC", &
             "7C 3C TH 5S 8H 8C 9C JD TC KD", &
             "QC TC JD TS 8C 3H 6H KD 7C TD", &
             "JH QS KS 9C 6D 6S AS 9H KH 6H", &
             "2H 4D AH 2D JH 6H TD 5D 4H JD", &
             "KD 8C 9S JH QD JS 2C QS 5C 7C", &
             "4S TC 7H 8D 2S 6H 7S 9C 7C KC", &
             "8C 5D 7H 4S TD QC 8S JS 4H KS", &
             "AD 8S JH 6D TD KD 7C 6C 2D 7D", &
             "JC 6H 6S JS 4H QH 9H AH 4C 3C", &
             "6H 5H AS 7C 7S 3D KH KC 5D 5C", &
             "JC 3D TD AS 4D 6D 6S QH JD KS", &
             "8C 7S 8S QH 2S JD 5C 7H AH QD", &
             "8S 3C 6H 6C 2C 8D TD 7D 4C 4D", &
             "5D QH KH 7C 2S 7H JS 6D QC QD", &
             "AD 6C 6S 7D TH 6H 2H 8H KH 4H", &
             "KS JS KD 5D 2D KH 7D 9C 8C 3D", &
             "9C 6D QD 3C KS 3S 7S AH JD 2D", &
             "AH QH AS JC 8S 8H 4C KC TH 7D", &
             "JC 5H TD 7C 5D KD 4C AD 8H JS", &
             "KC 2H AC AH 7D JH KH 5D 7S 6D", &
             "9S 5S 9C 6H 8S TD JD 9H 6C AC", &
             "7D 8S 6D TS KD 7H AC 5S 7C 5D", &
             "AH QC JC 4C TC 8C 2H TS 2C 7D", &
             "KD KC 6S 3D 7D 2S 8S 3H 5S 5C", &
             "8S 5D 8H 4C 6H KC 3H 7C 5S KD", &
             "JH 8C 3D 3C 6C KC TD 7H 7C 4C", &
             "JC KC 6H TS QS TD KS 8H 8C 9S", &
             "6C 5S 9C QH 7D AH KS KC 9S 2C", &
             "4D 4S 8H TD 9C 3S 7D 9D AS TH", &
             "6S 7D 3C 6H 5D KD 2C 5C 9D 9C", &
             "2H KC 3D AD 3H QD QS 8D JC 4S", &
             "8C 3H 9C 7C AD 5D JC 9D JS AS", &
             "5D 9H 5C 7H 6S 6C QC JC QD 9S", &
             "JC QS JH 2C 6S 9C QC 3D 4S TC", &
             "4H 5S 8D 3D 4D 2S KC 2H JS 2C", &
             "TD 3S TH KD 4D 7H JH JS KS AC", &
             "7S 8C 9S 2D 8S 7D 5C AD 9D AS", &
             "8C 7H 2S 6C TH 3H 4C 3S 8H AC", &
             "KD 5H JC 8H JD 2D 4H TD JH 5C", &
             "3D AS QH KS 7H JD 8S 5S 6D 5H", &
             "9S 6S TC QS JC 5C 5D 9C TH 8C", &
             "5H 3S JH 9H 2S 2C 6S 7S AS KS", &
             "8C QD JC QS TC QC 4H AC KH 6C", &
             "TC 5H 7D JH 4H 2H 8D JC KS 4D", &
             "5S 9C KH KD 9H 5C TS 3D 7D 2D", &
             "5H AS TC 4D 8C 2C TS 9D 3H 8D", &
             "6H 8D 2D 9H JD 6C 4S 5H 5S 6D", &
             "AD 9C JC 7D 6H 9S 6D JS 9H 3C", &
             "AD JH TC QS 4C 5D 9S 7C 9C AH", &
             "KD 6H 2H TH 8S QD KS 9D 9H AS", &
             "4H 8H 8D 5H 6C AH 5S AS AD 8S", &
             "QS 5D 4S 2H TD KS 5H AC 3H JC", &
             "9C 7D QD KD AC 6D 5H QH 6H 5S", &
             "KC AH QH 2H 7D QS 3H KS 7S JD", &
             "6C 8S 3H 6D KS QD 5D 5C 8H TC", &
             "9H 4D 4S 6S 9D KH QC 4H 6C JD", &
             "TD 2D QH 4S 6H JH KD 3C QD 8C", &
             "4S 6H 7C QD 9D AS AH 6S AD 3C", &
             "2C KC TH 6H 8D AH 5C 6D 8S 5D", &
             "TD TS 7C AD JC QD 9H 3C KC 7H", &
             "5D 4D 5S 8H 4H 7D 3H JD KD 2D", &
             "JH TD 6H QS 4S KD 5C 8S 7D 8H", &
             "AC 3D AS 8C TD 7H KH 5D 6C JD", &
             "9D KS 7C 6D QH TC JD KD AS KC", &
             "JH 8S 5S 7S 7D AS 2D 3D AD 2H", &
             "2H 5D AS 3C QD KC 6H 9H 9S 2C", &
             "9D 5D TH 4C JH 3H 8D TC 8H 9H", &
             "6H KD 2C TD 2H 6C 9D 2D JS 8C", &
             "KD 7S 3C 7C AS QH TS AD 8C 2S", &
             "QS 8H 6C JS 4C 9S QC AD TD TS", &
             "2H 7C TS TC 8C 3C 9H 2D 6D JC", &
             "TC 2H 8D JH KS 6D 3H TD TH 8H", &
             "9D TD 9H QC 5D 6C 8H 8C KC TS", &
             "2H 8C 3D AH 4D TH TC 7D 8H KC", &
             "TS 5C 2D 8C 6S KH AH 5H 6H KC", &
             "5S 5D AH TC 4C JD 8D 6H 8C 6C", &
             "KC QD 3D 8H 2D JC 9H 4H AD 2S", &
             "TD 6S 7D JS KD 4H QS 2S 3S 8C", &
             "4C 9H JH TS 3S 4H QC 5S 9S 9C", &
             "2C KD 9H JS 9S 3H JC TS 5D AC", &
             "AS 2H 5D AD 5H JC 7S TD JS 4C", &
             "2D 4S 8H 3D 7D 2C AD KD 9C TS", &
             "7H QD JH 5H JS AC 3D TH 4C 8H", &
             "6D KH KC QD 5C AD 7C 2D 4H AC", &
             "3D 9D TC 8S QD 2C JC 4H JD AH", &
             "6C TD 5S TC 8S AH 2C 5D AS AC", &
             "TH 7S 3D AS 6C 4C 7H 7D 4H AH", &
             "5C 2H KS 6H 7S 4H 5H 3D 3C 7H", &
             "3C 9S AC 7S QH 2H 3D 6S 3S 3H", &
             "2D 3H AS 2C 6H TC JS 6S 9C 6C", &
             "QH KD QD 6D AC 6H KH 2C TS 8C", &
             "8H 7D 3S 9H 5D 3H 4S QC 9S 5H", &
             "2D 9D 7H 6H 3C 8S 5H 4D 3S 4S", &
             "KD 9S 4S TC 7S QC 3S 8S 2H 7H", &
             "TC 3D 8C 3H 6C 2H 6H KS KD 4D", &
             "KC 3D 9S 3H JS 4S 8H 2D 6C 8S", &
             "6H QS 6C TC QD 9H 7D 7C 5H 4D", &
             "TD 9D 8D 6S 6C TC 5D TS JS 8H", &
             "4H KC JD 9H TC 2C 6S 5H 8H AS", &
             "JS 9C 5C 6S 9D JD 8H KC 4C 6D", &
             "4D 8D 8S 6C 7C 6H 7H 8H 5C KC", &
             "TC 3D JC 6D KS 9S 6H 7S 9C 2C", &
             "6C 3S KD 5H TS 7D 9H 9S 6H KH", &
             "3D QD 4C 6H TS AC 3S 5C 2H KD", &
             "4C AS JS 9S 7C TS 7H 9H JC KS", &
             "4H 8C JD 3H 6H AD 9S 4S 5S KS", &
             "4C 2C 7D 3D AS 9C 2S QS KC 6C", &
             "8S 5H 3D 2S AC 9D 6S 3S 4D TD", &
             "QD TH 7S TS 3D AC 7H 6C 5D QC", &
             "TC QD AD 9C QS 5C 8D KD 3D 3C", &
             "9D 8H AS 3S 7C 8S JD 2D 8D KC", &
             "4C TH AC QH JS 8D 7D 7S 9C KH", &
             "9D 8D 4C JH 2C 2S QD KD TS 4H", &
             "4D 6D 5D 2D JH 3S 8S 3H TC KH", &
             "AD 4D 2C QS 8C KD JH JD AH 5C", &
             "5C 6C 5H 2H JH 4H KS 7C TC 3H", &
             "3C 4C QC 5D JH 9C QD KH 8D TC", &
             "3H 9C JS 7H QH AS 7C 9H 5H JC", &
             "2D 5S QD 4S 3C KC 6S 6C 5C 4C", &
             "5D KH 2D TS 8S 9C AS 9S 7C 4C", &
             "7C AH 8C 8D 5S KD QH QS JH 2C", &
             "8C 9D AH 2H AC QC 5S 8H 7H 2C", &
             "QD 9H 5S QS QC 9C 5H JC TH 4H", &
             "6C 6S 3H 5H 3S 6H KS 8D AC 7S", &
             "AC QH 7H 8C 4S KC 6C 3D 3S TC", &
             "9D 3D JS TH AC 5H 3H 8S 3S TC", &
             "QD KH JS KS 9S QC 8D AH 3C AC", &
             "5H 6C KH 3S 9S JH 2D QD AS 8C", &
             "6C 4D 7S 7H 5S JC 6S 9H 4H JH", &
             "AH 5S 6H 9S AD 3S TH 2H 9D 8C", &
             "4C 8D 9H 7C QC AD 4S 9C KC 5S", &
             "9D 6H 4D TC 4C JH 2S 5D 3S AS", &
             "2H 6C 7C KH 5C AD QS TH JD 8S", &
             "3S 4S 7S AH AS KC JS 2S AD TH", &
             "JS KC 2S 7D 8C 5C 9C TS 5H 9D", &
             "7S 9S 4D TD JH JS KH 6H 5D 2C", &
             "JD JS JC TH 2D 3D QD 8C AC 5H", &
             "7S KH 5S 9D 5D TD 4S 6H 3C 2D", &
             "4S 5D AC 8D 4D 7C AD AS AH 9C", &
             "6S TH TS KS 2C QC AH AS 3C 4S", &
             "2H 8C 3S JC 5C 7C 3H 3C KH JH", &
             "7S 3H JC 5S 6H 4C 2S 4D KC 7H", &
             "4D 7C 4H 9S 8S 6S AD TC 6C JC", &
             "KH QS 3S TC 4C 8H 8S AC 3C TS", &
             "QD QS TH 3C TS 7H 7D AH TD JC", &
             "TD JD QC 4D 9S 7S TS AD 7D AC", &
             "AH 7H 4S 6D 7C 2H 9D KS JC TD", &
             "7C AH JD 4H 6D QS TS 2H 2C 5C", &
             "TC KC 8C 9S 4C JS 3C JC 6S AH", &
             "AS 7D QC 3D 5S JC JD 9D TD KH", &
             "TH 3C 2S 6H AH AC 5H 5C 7S 8H", &
             "QC 2D AC QD 2S 3S JD QS 6S 8H", &
             "KC 4H 3C 9D JS 6H 3S 8S AS 8C", &
             "7H KC 7D JD 2H JC QH 5S 3H QS", &
             "9H TD 3S 8H 7S AC 5C 6C AH 7C", &
             "8D 9H AH JD TD QS 7D 3S 9C 8S", &
             "AH QH 3C JD KC 4S 5S 5D TD KS", &
             "9H 7H 6S JH TH 4C 7C AD 5C 2D", &
             "7C KD 5S TC 9D 6S 6C 5D 2S TH", &
             "KC 9H 8D 5H 7H 4H QC 3D 7C AS", &
             "6S 8S QC TD 4S 5C TH QS QD 2S", &
             "8S 5H TH QC 9H 6S KC 7D 7C 5C", &
             "7H KD AH 4D KH 5C 4S 2D KC QH", &
             "6S 2C TD JC AS 4D 6C 8C 4H 5S", &
             "JC TC JD 5S 6S 8D AS 9D AD 3S", &
             "6D 6H 5D 5S TC 3D 7D QS 9D QD", &
             "4S 6C 8S 3S 7S AD KS 2D 7D 7C", &
             "KC QH JC AC QD 5D 8D QS 7H 7D", &
             "JS AH 8S 5H 3D TD 3H 4S 6C JH", &
             "4S QS 7D AS 9H JS KS 6D TC 5C", &
             "2D 5C 6H TC 4D QH 3D 9H 8S 6C", &
             "6D 7H TC TH 5S JD 5C 9C KS KD", &
             "8D TD QH 6S 4S 6C 8S KC 5C TC", &
             "5S 3D KS AC 4S 7D QD 4C TH 2S", &
             "TS 8H 9S 6S 7S QH 3C AH 7H 8C", &
             "4C 8C TS JS QC 3D 7D 5D 7S JH", &
             "8S 7S 9D QC AC 7C 6D 2H JH KC", &
             "JS KD 3C 6S 4S 7C AH QC KS 5H", &
             "KS 6S 4H JD QS TC 8H KC 6H AS", &
             "KH 7C TC 6S TD JC 5C 7D AH 3S", &
             "3H 4C 4H TC TH 6S 7H 6D 9C QH", &
             "7D 5H 4S 8C JS 4D 3D 8S QH KC", &
             "3H 6S AD 7H 3S QC 8S 4S 7S JS", &
             "3S JD KH TH 6H QS 9C 6C 2D QD", &
             "4S QH 4D 5H KC 7D 6D 8D TH 5S", &
             "TD AD 6S 7H KD KH 9H 5S KC JC", &
             "3H QC AS TS 4S QD KS 9C 7S KC", &
             "TS 6S QC 6C TH TC 9D 5C 5D KD", &
             "JS 3S 4H KD 4C QD 6D 9S JC 9D", &
             "8S JS 6D 4H JH 6H 6S 6C KS KH", &
             "AC 7D 5D TC 9S KH 6S QD 6H AS", &
             "AS 7H 6D QH 8D TH 2S KH 5C 5H", &
             "4C 7C 3D QC TC 4S KH 8C 2D JS", &
             "6H 5D 7S 5H 9C 9H JH 8S TH 7H", &
             "AS JS 2S QD KH 8H 4S AC 8D 8S", &
             "3H 4C TD KD 8C JC 5C QS 2D JD", &
             "TS 7D 5D 6C 2C QS 2H 3C AH KS", &
             "4S 7C 9C 7D JH 6C 5C 8H 9D QD", &
             "2S TD 7S 6D 9C 9S QS KH QH 5C", &
             "JC 6S 9C QH JH 8D 7S JS KH 2H", &
             "8D 5H TH KC 4D 4S 3S 6S 3D QS", &
             "2D JD 4C TD 7C 6D TH 7S JC AH", &
             "QS 7S 4C TH 9D TS AD 4D 3H 6H", &
             "2D 3H 7D JD 3D AS 2S 9C QC 8S", &
             "4H 9H 9C 2C 7S JH KD 5C 5D 6H", &
             "TC 9H 8H JC 3C 9S 8D KS AD KC", &
             "TS 5H JD QS QH QC 8D 5D KH AH", &
             "5D AS 8S 6S 4C AH QC QD TH 7H", &
             "3H 4H 7D 6S 4S 9H AS 8H JS 9D", &
             "JD 8C 2C 9D 7D 5H 5S 9S JC KD", &
             "KD 9C 4S QD AH 7C AD 9D AC TD", &
             "6S 4H 4S 9C 8D KS TC 9D JH 7C", &
             "5S JC 5H 4S QH AC 2C JS 2S 9S", &
             "8C 5H AS QD AD 5C 7D 8S QC TD", &
             "JC 4C 8D 5C KH QS 4D 6H 2H 2C", &
             "TH 4S 2D KC 3H QD AC 7H AD 9D", &
             "KH QD AS 8H TH KC 8D 7S QH 8C", &
             "JC 6C 7D 8C KH AD QS 2H 6S 2D", &
             "JC KH 2D 7D JS QC 5H 4C 5D AD", &
             "TS 3S AD 4S TD 2D TH 6S 9H JH", &
             "9H 2D QS 2C 4S 3D KH AS AC 9D", &
             "KH 6S 8H 4S KD 7D 9D TS QD QC", &
             "JH 5H AH KS AS AD JC QC 5S KH", &
             "5D 7D 6D KS KD 3D 7C 4D JD 3S", &
             "AC JS 8D 5H 9C 3H 4H 4D TS 2C", &
             "6H KS KH 9D 7C 2S 6S 8S 2H 3D", &
             "6H AC JS 7S 3S TD 8H 3H 4H TH", &
             "9H TC QC KC 5C KS 6H 4H AC 8S", &
             "TC 7D QH 4S JC TS 6D 6C AC KH", &
             "QH 7D 7C JH QS QD TH 3H 5D KS", &
             "3D 5S 8D JS 4C 2C KS 7H 9C 4H", &
             "5H 8S 4H TD 2C 3S QD QC 3H KC", &
             "QC JS KD 9C AD 5S 9D 7D 7H TS", &
             "8C JC KH 7C 7S 6C TS 2C QD TH", &
             "5S 9D TH 3C 7S QH 8S 9C 2H 5H", &
             "5D 9H 6H 2S JS KH 3H 7C 2H 5S", &
             "JD 5D 5S 2C TC 2S 6S 6C 3C 8S", &
             "4D KH 8H 4H 2D KS 3H 5C 2S 9H", &
             "3S 2D TD 7H 8S 6H JD KC 9C 8D", &
             "6S QD JH 7C 9H 5H 8S 8H TH TD", &
             "QS 7S TD 7D TS JC KD 7C 3C 2C", &
             "3C JD 8S 4H 2D 2S TD AS 4D AC", &
             "AH KS 6C 4C 4S 7D 8C 9H 6H AS", &
             "5S 3C 9S 2C QS KD 4D 4S AC 5D", &
             "2D TS 2C JS KH QH 5D 8C AS KC", &
             "KD 3H 6C TH 8S 7S KH 6H 9S AC", &
             "6H 7S 6C QS AH 2S 2H 4H 5D 5H", &
             "5H JC QD 2C 2S JD AS QC 6S 7D", &
             "6C TC AS KD 8H 9D 2C 7D JH 9S", &
             "2H 4C 6C AH 8S TD 3H TH 7C TS", &
             "KD 4S TS 6C QH 8D 9D 9C AH 7D", &
             "6D JS 5C QD QC 9C 5D 8C 2H KD", &
             "3C QH JH AD 6S AH KC 8S 6D 6H", &
             "3D 7C 4C 7S 5S 3S 6S 5H JC 3C", &
             "QH 7C 5H 3C 3S 8C TS 4C KD 9C", &
             "QD 3S 7S 5H 7H QH JC 7C 8C KD", &
             "3C KD KH 2S 4C TS AC 6S 2C 7C", &
             "2C KH 3C 4C 6H 4D 5H 5S 7S QD", &
             "4D 7C 8S QD TS 9D KS 6H KD 3C", &
             "QS 4D TS 7S 4C 3H QD 8D 9S TC", &
             "TS QH AC 6S 3C 9H 9D QS 8S 6H", &
             "3S 7S 5D 4S JS 2D 6C QH 6S TH", &
             "4C 4H AS JS 5D 3D TS 9C AC 8S", &
             "6S 9C 7C 3S 5C QS AD AS 6H 3C", &
             "9S 8C 7H 3H 6S 7C AS 9H JD KH", &
             "3D 3H 7S 4D 6C 7C AC 2H 9C TH", &
             "4H 5S 3H AC TC TH 9C 9H 9S 8D", &
             "8D 9H 5H 4D 6C 2H QD 6S 5D 3S", &
             "4C 5C JD QS 4D 3H TH AC QH 8C", &
             "QC 5S 3C 7H AD 4C KS 4H JD 6D", &
             "QS AH 3H KS 9H 2S JS JH 5H 2H", &
             "2H 5S TH 6S TS 3S KS 3C 5H JS", &
             "2D 9S 7H 3D KC JH 6D 7D JS TD", &
             "AC JS 8H 2C 8C JH JC 2D TH 7S", &
             "5D 9S 8H 2H 3D TC AH JC KD 9C", &
             "9D QD JC 2H 6D KH TS 9S QH TH", &
             "2C 8D 4S JD 5H 3H TH TC 9C KC", &
             "AS 3D 9H 7D 4D TH KH 2H 7S 3H", &
             "4H 7S KS 2S JS TS 8S 2H QD 8D", &
             "5S 6H JH KS 8H 2S QC AC 6S 3S", &
             "JC AS AD QS 8H 6C KH 4C 4D QD", &
             "2S 3D TS TD 9S KS 6S QS 5C 8D", &
             "3C 6D 4S QC KC JH QD TH KH AD", &
             "9H AH 4D KS 2S 8D JH JC 7C QS", &
             "2D 6C TH 3C 8H QD QH 2S 3S KS", &
             "6H 5D 9S 4C TS TD JS QD 9D JD", &
             "5H 8H KH 8S KS 7C TD AD 4S KD", &
             "2C 7C JC 5S AS 6C 7D 8S 5H 9C", &
             "6S QD 9S TS KH QS 5S QH 3C KC", &
             "7D 3H 3C KD 5C AS JH 7H 6H JD", &
             "9D 5C 9H KC 8H KS 4S AD 4D 2S", &
             "3S JD QD 8D 2S 7C 5S 6S 5H TS", &
             "6D 9S KC TD 3S 6H QD JD 5C 8D", &
             "5H 9D TS KD 8D 6H TD QC 4C 7D", &
             "6D 4S JD 9D AH 9S AS TD 9H QD", &
             "2D 5S 2H 9C 6H 9S TD QC 7D TC", &
             "3S 2H KS TS 2C 9C 8S JS 9D 7D", &
             "3C KC 6D 5D 6C 6H 8S AS 7S QS", &
             "JH 9S 2H 8D 4C 8H 9H AD TH KH", &
             "QC AS 2S JS 5C 6H KD 3H 7H 2C", &
             "QD 8H 2S 8D 3S 6D AH 2C TC 5C", &
             "JD JS TS 8S 3H 5D TD KC JC 6H", &
             "6S QS TC 3H 5D AH JC 7C 7D 4H", &
             "7C 5D 8H 9C 2H 9H JH KH 5S 2C", &
             "9C 7H 6S TH 3S QC QD 4C AC JD", &
             "2H 5D 9S 7D KC 3S QS 2D AS KH", &
             "2S 4S 2H 7D 5C TD TH QH 9S 4D", &
             "6D 3S TS 6H 4H KS 9D 8H 5S 2D", &
             "9H KS 4H 3S 5C 5D KH 6H 6S JS", &
             "KC AS 8C 4C JC KH QC TH QD AH", &
             "6S KH 9S 2C 5H TC 3C 7H JC 4D", &
             "JD 4S 6S 5S 8D 7H 7S 4D 4C 2H", &
             "7H 9H 5D KH 9C 7C TS TC 7S 5H", &
             "4C 8D QC TS 4S 9H 3D AD JS 7C", &
             "8C QS 5C 5D 3H JS AH KC 4S 9D", &
             "TS JD 8S QS TH JH KH 2D QD JS", &
             "JD QC 5D 6S 9H 3S 2C 8H 9S TS", &
             "2S 4C AD 7H JC 5C 2D 6D 4H 3D", &
             "7S JS 2C 4H 8C AD QD 9C 3S TD", &
             "JD TS 4C 6H 9H 7D QD 6D 3C AS", &
             "AS 7C 4C 6S 5D 5S 5C JS QC 4S", &
             "KD 6S 9S 7C 3C 5S 7D JH QD JS", &
             "4S 7S JH 2C 8S 5D 7H 3D QH AD", &
             "TD 6H 2H 8D 4H 2D 7C AD KH 5D", &
             "TS 3S 5H 2C QD AH 2S 5C KH TD", &
             "KC 4D 8C 5D AS 6C 2H 2S 9H 7C", &
             "KD JS QC TS QS KH JH 2C 5D AD", &
             "3S 5H KC 6C 9H 3H 2H AD 7D 7S", &
             "7S JS JH KD 8S 7D 2S 9H 7C 2H", &
             "9H 2D 8D QC 6S AD AS 8H 5H 6C", &
             "2S 7H 6C 6D 7D 8C 5D 9D JC 3C", &
             "7C 9C 7H JD 2H KD 3S KH AD 4S", &
             "QH AS 9H 4D JD KS KD TS KH 5H", &
             "4C 8H 5S 3S 3D 7D TD AD 7S KC", &
             "JS 8S 5S JC 8H TH 9C 4D 5D KC", &
             "7C 5S 9C QD 2C QH JS 5H 8D KH", &
             "TD 2S KS 3D AD KC 7S TC 3C 5D", &
             "4C 2S AD QS 6C 9S QD TH QH 5C", &
             "8C AD QS 2D 2S KC JD KS 6C JC", &
             "8D 4D JS 2H 5D QD 7S 7D QH TS", &
             "6S 7H 3S 8C 8S 9D QS 8H 6C 9S", &
             "4S TC 2S 5C QD 4D QS 6D TH 6S", &
             "3S 5C 9D 6H 8D 4C 7D TC 7C TD", &
             "AH 6S AS 7H 5S KD 3H 5H AC 4C", &
             "8D 8S AH KS QS 2C AD 6H 7D 5D", &
             "6H 9H 9S 2H QS 8S 9C 5D 2D KD", &
             "TS QC 5S JH 7D 7S TH 9S 9H AC", &
             "7H 3H 6S KC 4D 6D 5C 4S QD TS", &
             "TD 2S 7C QD 3H JH 9D 4H 7S 7H", &
             "KS 3D 4H 5H TC 2S AS 2D 6D 7D", &
             "8H 3C 7H TD 3H AD KC TH 9C KH", &
             "TC 4C 2C 9S 9D 9C 5C 2H JD 3C", &
             "3H AC TS 5D AD 8D 6H QC 6S 8C", &
             "2S TS 3S JD 7H 8S QH 4C 5S 8D", &
             "AC 4S 6C 3C KH 3D 7C 2D 8S 2H", &
             "4H 6C 8S TH 2H 4S 8H 9S 3H 7S", &
             "7C 4C 9C 2C 5C AS 5D KD 4D QH", &
             "9H 4H TS AS 7D 8D 5D 9S 8C 2H", &
             "QC KD AC AD 2H 7S AS 3S 2D 9S", &
             "2H QC 8H TC 6D QD QS 5D KH 3C", &
             "TH JD QS 4C 2S 5S AD 7H 3S AS", &
             "7H JS 3D 6C 3S 6D AS 9S AC QS", &
             "9C TS AS 8C TC 8S 6H 9D 8D 6C", &
             "4D JD 9C KC 7C 6D KS 3S 8C AS", &
             "3H 6S TC 8D TS 3S KC 9S 7C AS", &
             "8C QC 4H 4S 8S 6C 3S TC AH AC", &
             "4D 7D 5C AS 2H 6S TS QC AD TC", &
             "QD QC 8S 4S TH 3D AH TS JH 4H", &
             "5C 2D 9S 2C 3H 3C 9D QD QH 7D", &
             "KC 9H 6C KD 7S 3C 4D AS TC 2D", &
             "3D JS 4D 9D KS 7D TH QC 3H 3C", &
             "8D 5S 2H 9D 3H 8C 4C 4H 3C TH", &
             "JC TH 4S 6S JD 2D 4D 6C 3D 4C", &
             "TS 3S 2D 4H AC 2C 6S 2H JH 6H", &
             "TD 8S AD TC AH AC JH 9S 6S 7S", &
             "6C KC 4S JD 8D 9H 5S 7H QH AH", &
             "KD 8D TS JH 5C 5H 3H AD AS JS", &
             "2D 4H 3D 6C 8C 7S AD 5D 5C 8S", &
             "TD 5D 7S 9C 4S 5H 6C 8C 4C 8S", &
             "JS QH 9C AS 5C QS JC 3D QC 7C", &
             "JC 9C KH JH QS QC 2C TS 3D AD", &
             "5D JH AC 5C 9S TS 4C JD 8C KS", &
             "KC AS 2D KH 9H 2C 5S 4D 3D 6H", &
             "TH AH 2D 8S JC 3D 8C QH 7S 3S", &
             "8H QD 4H JC AS KH KS 3C 9S 6D", &
             "9S QH 7D 9C 4S AC 7H KH 4D KD", &
             "AH AD TH 6D 9C 9S KD KS QH 4H", &
             "QD 6H 9C 7C QS 6D 6S 9D 5S JH", &
             "AH 8D 5H QD 2H JC KS 4H KH 5S", &
             "5C 2S JS 8D 9C 8C 3D AS KC AH", &
             "JD 9S 2H QS 8H 5S 8C TH 5C 4C", &
             "QC QS 8C 2S 2C 3S 9C 4C KS KH", &
             "2D 5D 8S AH AD TD 2C JS KS 8C", &
             "TC 5S 5H 8H QC 9H 6H JD 4H 9S", &
             "3C JH 4H 9H AH 4S 2H 4C 8D AC", &
             "8S TH 4D 7D 6D QD QS 7S TC 7C", &
             "KH 6D 2D JD 5H JS QD JH 4H 4S", &
             "9C 7S JH 4S 3S TS QC 8C TC 4H", &
             "QH 9D 4D JH QS 3S 2C 7C 6C 2D", &
             "4H 9S JD 5C 5H AH 9D TS 2D 4C", &
             "KS JH TS 5D 2D AH JS 7H AS 8D", &
             "JS AH 8C AD KS 5S 8H 2C 6C TH", &
             "2H 5D AD AC KS 3D 8H TS 6H QC", &
             "6D 4H TS 9C 5H JS JH 6S JD 4C", &
             "JH QH 4H 2C 6D 3C 5D 4C QS KC", &
             "6H 4H 6C 7H 6S 2S 8S KH QC 8C", &
             "3H 3D 5D KS 4H TD AD 3S 4D TS", &
             "5S 7C 8S 7D 2C KS 7S 6C 8C JS", &
             "5D 2H 3S 7C 5C QD 5H 6D 9C 9H", &
             "JS 2S KD 9S 8D TD TS AC 8C 9D", &
             "5H QD 2S AC 8C 9H KS 7C 4S 3C", &
             "KH AS 3H 8S 9C JS QS 4S AD 4D", &
             "AS 2S TD AD 4D 9H JC 4C 5H QS", &
             "5D 7C 4H TC 2D 6C JS 4S KC 3S", &
             "4C 2C 5D AC 9H 3D JD 8S QS QH", &
             "2C 8S 6H 3C QH 6D TC KD AC AH", &
             "QC 6C 3S QS 4S AC 8D 5C AD KH", &
             "5S 4C AC KH AS QC 2C 5C 8D 9C", &
             "8H JD 3C KH 8D 5C 9C QD QH 9D", &
             "7H TS 2C 8C 4S TD JC 9C 5H QH", &
             "JS 4S 2C 7C TH 6C AS KS 7S JD", &
             "JH 7C 9H 7H TC 5H 3D 6D 5D 4D", &
             "2C QD JH 2H 9D 5S 3D TD AD KS", &
             "JD QH 3S 4D TH 7D 6S QS KS 4H", &
             "TC KS 5S 8D 8H AD 2S 2D 4C JH", &
             "5S JH TC 3S 2D QS 9D 4C KD 9S", &
             "AC KH 3H AS 9D KC 9H QD 6C 6S", &
             "9H 7S 3D 5C 7D KC TD 8H 4H 6S", &
             "3C 7H 8H TC QD 4D 7S 6S QH 6C", &
             "6D AD 4C QD 6C 5D 7D 9D KS TS", &
             "JH 2H JD 9S 7S TS KH 8D 5D 8H", &
             "2D 9S 4C 7D 9D 5H QD 6D AC 6S", &
             "7S 6D JC QD JH 4C 6S QS 2H 7D", &
             "8C TD JH KD 2H 5C QS 2C JS 7S", &
             "TC 5H 4H JH QD 3S 5S 5D 8S KH", &
             "KS KH 7C 2C 5D JH 6S 9C 6D JC", &
             "5H AH JD 9C JS KC 2H 6H 4D 5S", &
             "AS 3C TH QC 6H 9C 8S 8C TD 7C", &
             "KC 2C QD 9C KH 4D 7S 3C TS 9H", &
             "9C QC 2S TS 8C TD 9S QD 3S 3C", &
             "4D 9D TH JH AH 6S 2S JD QH JS", &
             "QD 9H 6C KD 7D 7H 5D 6S 8H AH", &
             "8H 3C 4S 2H 5H QS QH 7S 4H AC", &
             "QS 3C 7S 9S 4H 3S AH KS 9D 7C", &
             "AD 5S 6S 2H 2D 5H TC 4S 3C 8C", &
             "QH TS 6S 4D JS KS JH AS 8S 6D", &
             "2C 8S 2S TD 5H AS TC TS 6C KC", &
             "KC TS 8H 2H 3H 7C 4C 5S TH TD", &
             "KD AD KH 7H 7S 5D 5H 5S 2D 9C", &
             "AD 9S 3D 7S 8C QC 7C 9C KD KS", &
             "3C QC 9S 8C 4D 5C AS QD 6C 2C", &
             "2H KC 8S JD 7S AC 8D 5C 2S 4D", &
             "9D QH 3D 2S TC 3S KS 3C 9H TD", &
             "KD 6S AC 2C 7H 5H 3S 6C 6H 8C", &
             "QH TC 8S 6S KH TH 4H 5D TS 4D", &
             "8C JS 4H 6H 2C 2H 7D AC QD 3D", &
             "QS KC 6S 2D 5S 4H TD 3H JH 4C", &
             "7S 5H 7H 8H KH 6H QS TH KD 7D", &
             "5H AD KD 7C KH 5S TD 6D 3C 6C", &
             "8C 9C 5H JD 7C KC KH 7H 2H 3S", &
             "7S 4H AD 4D 8S QS TH 3D 7H 5S", &
             "8D TC KS KD 9S 6D AD JD 5C 2S", &
             "7H 8H 6C QD 2H 6H 9D TC 9S 7C", &
             "8D 6D 4C 7C 6C 3C TH KH JS JH", &
             "5S 3S 8S JS 9H AS AD 8H 7S KD", &
             "JH 7C 2C KC 5H AS AD 9C 9S JS", &
             "AD AC 2C 6S QD 7C 3H TH KS KD", &
             "9D JD 4H 8H 4C KH 7S TS 8C KC", &
             "3S 5S 2H 7S 6H 7D KS 5C 6D AD", &
             "5S 8C 9H QS 7H 7S 2H 6C 7D TD", &
             "QS 5S TD AC 9D KC 3D TC 2D 4D", &
             "TD 2H 7D JD QD 4C 7H 5D KC 3D", &
             "4C 3H 8S KD QH 5S QC 9H TC 5H", &
             "9C QD TH 5H TS 5C 9H AH QH 2C", &
             "4D 6S 3C AC 6C 3D 2C 2H TD TH", &
             "AC 9C 5D QC 4D AD 8D 6D 8C KC", &
             "AD 3C 4H AC 8D 8H 7S 9S TD JC", &
             "4H 9H QH JS 2D TH TD TC KD KS", &
             "5S 6S 9S 8D TH AS KH 5H 5C 8S", &
             "JD 2S 9S 6S 5S 8S 5D 7S 7H 9D", &
             "5D 8C 4C 9D AD TS 2C 7D KD TC", &
             "8S QS 4D KC 5C 8D 4S KH JD KD", &
             "AS 5C AD QH 7D 2H 9S 7H 7C TC", &
             "2S 8S JD KH 7S 6C 6D AD 5D QC", &
             "9H 6H 3S 8C 8H AH TC 4H JS TD", &
             "2C TS 4D 7H 2D QC 9C 5D TH 7C", &
             "6C 8H QC 5D TS JH 5C 5H 9H 4S", &
             "2D QC 7H AS JS 8S 2H 4C 4H 8D", &
             "JS 6S AC KD 3D 3C 4S 7H TH KC", &
             "QH KH 6S QS 5S 4H 3C QD 3S 3H", &
             "7H AS KH 8C 4H 9C 5S 3D 6S TS", &
             "9C 7C 3H 5S QD 2C 3D AD AC 5H", &
             "JH TD 2D 4C TS 3H KH AD 3S 7S", &
             "AS 4C 5H 4D 6S KD JC 3C 6H 2D", &
             "3H 6S 8C 2D TH 4S AH QH AD 5H", &
             "7C 2S 9H 7H KC 5C 6D 5S 3H JC", &
             "3C TC 9C 4H QD TD JH 6D 9H 5S", &
             "7C 6S 5C 5D 6C 4S 7H 9H 6H AH", &
             "AD 2H 7D KC 2C 4C 2S 9S 7H 3S", &
             "TH 4C 8S 6S 3S AD KS AS JH TD", &
             "5C TD 4S 4D AD 6S 5D TC 9C 7D", &
             "8H 3S 4D 4S 5S 6H 5C AC 3H 3D", &
             "9H 3C AC 4S QS 8S 9D QH 5H 4D", &
             "JC 6C 5H TS AC 9C JD 8C 7C QD", &
             "8S 8H 9C JD 2D QC QH 6H 3C 8D", &
             "KS JS 2H 6H 5H QH QS 3H 7C 6D", &
             "TC 3H 4S 7H QC 2H 3S 8C JS KH", &
             "AH 8H 5S 4C 9H JD 3H 7S JC AC", &
             "3C 2D 4C 5S 6C 4S QS 3S JD 3D", &
             "5H 2D TC AH KS 6D 7H AD 8C 6H", &
             "6C 7S 3C JD 7C 8H KS KH AH 6D", &
             "AH 7D 3H 8H 8S 7H QS 5H 9D 2D", &
             "JD AC 4H 7S 8S 9S KS AS 9D QH", &
             "7S 2C 8S 5S JH QS JC AH KD 4C", &
             "AH 2S 9H 4H 8D TS TD 6H QH JD", &
             "4H JC 3H QS 6D 7S 9C 8S 9D 8D", &
             "5H TD 4S 9S 4C 8C 8D 7H 3H 3D", &
             "QS KH 3S 2C 2S 3C 7S TD 4S QD", &
             "7C TD 4D 5S KH AC AS 7H 4C 6C", &
             "2S 5H 6D JD 9H QS 8S 2C 2H TD", &
             "2S TS 6H 9H 7S 4H JC 4C 5D 5S", &
             "2C 5H 7D 4H 3S QH JC JS 6D 8H", &
             "4C QH 7C QD 3S AD TH 8S 5S TS", &
             "9H TC 2S TD JC 7D 3S 3D TH QH", &
             "7D 4C 8S 5C JH 8H 6S 3S KC 3H", &
             "JC 3H KH TC QH TH 6H 2C AC 5H", &
             "QS 2H 9D 2C AS 6S 6C 2S 8C 8S", &
             "9H 7D QC TH 4H KD QS AC 7S 3C", &
             "4D JH 6S 5S 8H KS 9S QC 3S AS", &
             "JD 2D 6S 7S TC 9H KC 3H 7D KD", &
             "2H KH 7C 4D 4S 3H JS QD 7D KC", &
             "4C JC AS 9D 3C JS 6C 8H QD 4D", &
             "AH JS 3S 6C 4C 3D JH 6D 9C 9H", &
             "9H 2D 8C 7H 5S KS 6H 9C 2S TC", &
             "6C 8C AD 7H 6H 3D KH AS 5D TH", &
             "KS 8C 3S TS 8S 4D 5S 9S 6C 4H", &
             "9H 4S 4H 5C 7D KC 2D 2H 9D JH", &
             "5C JS TC 9D 9H 5H 7S KH JC 6S", &
             "7C 9H 8H 4D JC KH JD 2H TD TC", &
             "8H 6C 2H 2C KH 6H 9D QS QH 5H", &
             "AC 7D 2S 3D QD JC 2D 8D JD JH", &
             "2H JC 2D 7H 2C 3C 8D KD TD 4H", &
             "3S 4H 6D 8D TS 3H TD 3D 6H TH", &
             "JH JC 3S AC QH 9H 7H 8S QC 2C", &
             "7H TD QS 4S 8S 9C 2S 5D 4D 2H", &
             "3D TS 3H 2S QC 8H 6H KC JC KS", &
             "5D JD 7D TC 8C 6C 9S 3D 8D AC", &
             "8H 6H JH 6C 5D 8D 8S 4H AD 2C", &
             "9D 4H 2D 2C 3S TS AS TC 3C 5D", &
             "4D TH 5H KS QS 6C 4S 2H 3D AD", &
             "5C KC 6H 2C 5S 3C 4D 2D 9H 9S", &
             "JD 4C 3H TH QH 9H 5S AH 8S AC", &
             "7D 9S 6S 2H TD 9C 4H 8H QS 4C", &
             "3C 6H 5D 4H 8C 9C KC 6S QD QS", &
             "3S 9H KD TC 2D JS 8C 6S 4H 4S", &
             "2S 4C 8S QS 6H KH 3H TH 8C 5D", &
             "2C KH 5S 3S 7S 7H 6C 9D QD 8D", &
             "8H KS AC 2D KH TS 6C JS KC 7H", &
             "9C KS 5C TD QC AH 6C 5H 9S 7C", &
             "5D 4D 3H 4H 6S 7C 7S AH QD TD", &
             "2H 7D QC 6S TC TS AH 7S 9D 3H", &
             "TH 5H QD 9S KS 7S 7C 6H 8C TD", &
             "TH 2D 4D QC 5C 7D JD AH 9C 4H", &
             "4H 3H AH 8D 6H QC QH 9H 2H 2C", &
             "2D AD 4C TS 6H 7S TH 4H QS TD", &
             "3C KD 2H 3H QS JD TC QC 5D 8H", &
             "KS JC QD TH 9S KD 8D 8C 2D 9C", &
             "3C QD KD 6D 4D 8D AH AD QC 8S", &
             "8H 3S 9D 2S 3H KS 6H 4C 7C KC", &
             "TH 9S 5C 3D 7D 6H AC 7S 4D 2C", &
             "5C 3D JD 4D 2D 6D 5H 9H 4C KH", &
             "AS 7H TD 6C 2H 3D QD KS 4C 4S", &
             "JC 3C AC 7C JD JS 8H 9S QC 5D", &
             "JD 6S 5S 2H AS 8C 7D 5H JH 3D", &
             "8D TC 5S 9S 8S 3H JC 5H 7S AS", &
             "5C TD 3D 7D 4H 8D 7H 4D 5D JS", &
             "QS 9C KS TD 2S 8S 5C 2H 4H AS", &
             "TH 7S 4H 7D 3H JD KD 5D 2S KC", &
             "JD 7H 4S 8H 4C JS 6H QH 5S 4H", &
             "2C QS 8C 5S 3H QC 2S 6C QD AD", &
             "8C 3D JD TC 4H 2H AD 5S AC 2S", &
             "5D 2C JS 2D AD 9D 3D 4C 4S JH", &
             "8D 5H 5D 6H 7S 4D KS 9D TD JD", &
             "3D 6D 9C 2S AS 7D 5S 5C 8H JD", &
             "7C 8S 3S 6S 5H JD TC AD 7H 7S", &
             "2S 9D TS 4D AC 8D 6C QD JD 3H", &
             "9S KH 2C 3C AC 3D 5H 6H 8D 5D", &
             "KS 3D 2D 6S AS 4C 2S 7C 7H KH", &
             "AC 2H 3S JC 5C QH 4D 2D 5H 7S", &
             "TS AS JD 8C 6H JC 8S 5S 2C 5D", &
             "7S QH 7H 6C QC 8H 2D 7C JD 2S", &
             "2C QD 2S 2H JC 9C 5D 2D JD JH", &
             "7C 5C 9C 8S 7D 6D 8D 6C 9S JH", &
             "2C AD 6S 5H 3S KS 7S 9D KH 4C", &
             "7H 6C 2C 5C TH 9D 8D 3S QC AH", &
             "5S KC 6H TC 5H 8S TH 6D 3C AH", &
             "9C KD 4H AD TD 9S 4S 7D 6H 5D", &
             "7H 5C 5H 6D AS 4C KD KH 4H 9D", &
             "3C 2S 5C 6C JD QS 2H 9D 7D 3H", &
             "AC 2S 6S 7S JS QD 5C QS 6H AD", &
             "5H TH QC 7H TC 3S 7C 6D KC 3D", &
             "4H 3D QC 9S 8H 2C 3S JC KS 5C", &
             "4S 6S 2C 6H 8S 3S 3D 9H 3H JS", &
             "4S 8C 4D 2D 8H 9H 7D 9D AH TS", &
             "9S 2C 9H 4C 8D AS 7D 3D 6D 5S", &
             "6S 4C 7H 8C 3H 5H JC AH 9D 9C", &
             "2S 7C 5S JD 8C 3S 3D 4D 7D 6S", &
             "3C KC 4S 5D 7D 3D JD 7H 3H 4H", &
             "9C 9H 4H 4D TH 6D QD 8S 9S 7S", &
             "2H AC 8S 4S AD 8C 2C AH 7D TC", &
             "TS 9H 3C AD KS TC 3D 8C 8H JD", &
             "QC 8D 2C 3C 7D 7C JD 9H 9C 6C", &
             "AH 6S JS JH 5D AS QC 2C JD TD", &
             "9H KD 2H 5D 2D 3S 7D TC AH TS", &
             "TD 8H AS 5D AH QC AC 6S TC 5H", &
             "KS 4S 7H 4D 8D 9C TC 2H 6H 3H", &
             "3H KD 4S QD QH 3D 8H 8C TD 7S", &
             "8S JD TC AH JS QS 2D KH KS 4D", &
             "3C AD JC KD JS KH 4S TH 9H 2C", &
             "QC 5S JS 9S KS AS 7C QD 2S JD", &
             "KC 5S QS 3S 2D AC 5D 9H 8H KS", &
             "6H 9C TC AD 2C 6D 5S JD 6C 7C", &
             "QS KH TD QD 2C 3H 8S 2S QC AH", &
             "9D 9H JH TC QH 3C 2S JS 5C 7H", &
             "6C 3S 3D 2S 4S QD 2D TH 5D 2C", &
             "2D 6H 6D 2S JC QH AS 7H 4H KH", &
             "5H 6S KS AD TC TS 7C AC 4S 4H", &
             "AD 3C 4H QS 8C 9D KS 2H 2D 4D", &
             "4S 9D 6C 6D 9C AC 8D 3H 7H KD", &
             "JC AH 6C TS JD 6D AD 3S 5D QD", &
             "JC JH JD 3S 7S 8S JS QC 3H 4S", &
             "JD TH 5C 2C AD JS 7H 9S 2H 7S", &
             "8D 3S JH 4D QC AS JD 2C KC 6H", &
             "2C AC 5H KD 5S 7H QD JH AH 2D", &
             "JC QH 8D 8S TC 5H 5C AH 8C 6C", &
             "3H JS 8S QD JH 3C 4H 6D 5C 3S", &
             "6D 4S 4C AH 5H 5S 3H JD 7C 8D", &
             "8H AH 2H 3H JS 3C 7D QC 4H KD", &
             "6S 2H KD 5H 8H 2D 3C 8S 7S QD", &
             "2S 7S KC QC AH TC QS 6D 4C 8D", &
             "5S 9H 2C 3S QD 7S 6C 2H 7C 9D", &
             "3C 6C 5C 5S JD JC KS 3S 5D TS", &
             "7C KS 6S 5S 2S 2D TC 2H 5H QS", &
             "AS 7H 6S TS 5H 9S 9D 3C KD 2H", &
             "4S JS QS 3S 4H 7C 2S AC 6S 9D", &
             "8C JH 2H 5H 7C 5D QH QS KH QC", &
             "3S TD 3H 7C KC 8D 5H 8S KH 8C", &
             "4H KH JD TS 3C 7H AS QC JS 5S", &
             "AH 9D 2C 8D 4D 2D 6H 6C KC 6S", &
             "2S 6H 9D 3S 7H 4D KH 8H KD 3D", &
             "9C TC AC JH KH 4D JD 5H TD 3S", &
             "7S 4H 9D AS 4C 7D QS 9S 2S KH", &
             "3S 8D 8S KS 8C JC 5C KH 2H 5D", &
             "8S QH 2C 4D KC JS QC 9D AC 6H", &
             "8S 8C 7C JS JD 6S 4C 9C AC 4S", &
             "QH 5D 2C 7D JC 8S 2D JS JH 4C", &
             "JS 4C 7S TS JH KC KH 5H QD 4S", &
             "QD 8C 8D 2D 6S TD 9D AC QH 5S", &
             "QH QC JS 3D 3C 5C 4H KH 8S 7H", &
             "7C 2C 5S JC 8S 3H QC 5D 2H KC", &
             "5S 8D KD 6H 4H QD QH 6D AH 3D", &
             "7S KS 6C 2S 4D AC QS 5H TS JD", &
             "7C 2D TC 5D QS AC JS QC 6C KC", &
             "2C KS 4D 3H TS 8S AD 4H 7S 9S", &
             "QD 9H QH 5H 4H 4D KH 3S JC AD", &
             "4D AC KC 8D 6D 4C 2D KH 2C JD", &
             "2C 9H 2D AH 3H 6D 9C 7D TC KS", &
             "8C 3H KD 7C 5C 2S 4S 5H AS AH", &
             "TH JD 4H KD 3H TC 5C 3S AC KH", &
             "6D 7H AH 7S QC 6H 2D TD JD AS", &
             "JH 5D 7H TC 9S 7D JC AS 5S KH", &
             "2H 8C AD TH 6H QD KD 9H 6S 6C", &
             "QH KC 9D 4D 3S JS JH 4H 2C 9H", &
             "TC 7H KH 4H JC 7D 9S 3H QS 7S", &
             "AD 7D JH 6C 7H 4H 3S 3H 4D QH", &
             "JD 2H 5C AS 6C QC 4D 3C TC JH", &
             "AC JD 3H 6H 4C JC AD 7D 7H 9H", &
             "4H TC TS 2C 8C 6S KS 2H JD 9S", &
             "4C 3H QS QC 9S 9H 6D KC 9D 9C", &
             "5C AD 8C 2C QH TH QD JC 8D 8H", &
             "QC 2C 2S QD 9C 4D 3S 8D JH QS", &
             "9D 3S 2C 7S 7C JC TD 3C TC 9H", &
             "3C TS 8H 5C 4C 2C 6S 8D 7C 4H", &
             "KS 7H 2H TC 4H 2C 3S AS AH QS", &
             "8C 2D 2H 2C 4S 4C 6S 7D 5S 3S", &
             "TH QC 5D TD 3C QS KD KC KS AS", &
             "4D AH KD 9H KS 5C 4C 6H JC 7S", &
             "KC 4H 5C QS TC 2H JC 9S AH QH", &
             "4S 9H 3H 5H 3C QD 2H QC JH 8H", &
             "5D AS 7H 2C 3D JH 6H 4C 6S 7D", &
             "9C JD 9H AH JS 8S QH 3H KS 8H", &
             "3S AC QC TS 4D AD 3D AH 8S 9H", &
             "7H 3H QS 9C 9S 5H JH JS AH AC", &
             "8D 3C JD 2H AC 9C 7H 5S 4D 8H", &
             "7C JH 9H 6C JS 9S 7H 8C 9D 4H", &
             "2D AS 9S 6H 4D JS JH 9H AD QD", &
             "6H 7S JH KH AH 7H TD 5S 6S 2C", &
             "8H JH 6S 5H 5S 9D TC 4C QC 9S", &
             "7D 2C KD 3H 5H AS QD 7H JS 4D", &
             "TS QH 6C 8H TH 5H 3C 3H 9C 9D", &
             "AD KH JS 5D 3H AS AC 9S 5C KC", &
             "2C KH 8C JC QS 6D AH 2D KC TC", &
             "9D 3H 2S 7C 4D 6D KH KS 8D 7D", &
             "9H 2S TC JH AC QC 3H 5S 3S 8H", &
             "3S AS KD 8H 4C 3H 7C JH QH TS", &
             "7S 6D 7H 9D JH 4C 3D 3S 6C AS", &
             "4S 2H 2C 4C 8S 5H KC 8C QC QD", &
             "3H 3S 6C QS QC 2D 6S 5D 2C 9D", &
             "2H 8D JH 2S 3H 2D 6C 5C 7S AD", &
             "9H JS 5D QH 8S TS 2H 7S 6S AD", &
             "6D QC 9S 7H 5H 5C 7D KC JD 4H", &
             "QC 5S 9H 9C 4D 6S KS 2S 4C 7C", &
             "9H 7C 4H 8D 3S 6H 5C 8H JS 7S", &
             "2D 6H JS TD 4H 4D JC TH 5H KC", &
             "AC 7C 8D TH 3H 9S 2D 4C KC 4D", &
             "KD QS 9C 7S 3D KS AD TS 4C 4H", &
             "QH 9C 8H 2S 7D KS 7H 5D KD 4C", &
             "9C 2S 2H JC 6S 6C TC QC JH 5C", &
             "7S AC 8H KC 8S 6H QS JC 3D 6S", &
             "JS 2D JH 8C 4S 6H 8H 6D 5D AD", &
             "6H 7D 2S 4H 9H 7C AS AC 8H 5S", &
             "3C JS 4S 6D 5H 2S QH 6S 9C 2C", &
             "3D 5S 6S 9S 4C QS 8D QD 8S TC", &
             "9C 3D AH 9H 5S 2C 7D AD JC 3S", &
             "7H TC AS 3C 6S 6D 7S KH KC 9H", &
             "3S TC 8H 6S 5H JH 8C 7D AC 2S", &
             "QD 9D 9C 3S JC 8C KS 8H 5D 4D", &
             "JS AH JD 6D 9D 8C 9H 9S 8H 3H", &
             "2D 6S 4C 4D 8S AD 4S TC AH 9H", &
             "TS AC QC TH KC 6D 4H 7S 8C 2H", &
             "3C QD JS 9D 5S JC AH 2H TS 9H", &
             "3H 4D QH 5D 9C 5H 7D 4S JC 3S", &
             "8S TH 3H 7C 2H JD JS TS AC 8D", &
             "9C 2H TD KC JD 2S 8C 5S AD 2C", &
             "3D KD 7C 5H 4D QH QD TC 6H 7D", &
             "7H 2C KC 5S KD 6H AH QC 7S QH", &
             "6H 5C AC 5H 2C 9C 2D 7C TD 2S", &
             "4D 9D AH 3D 7C JD 4H 8C 4C KS", &
             "TH 3C JS QH 8H 4C AS 3D QS QC", &
             "4D 7S 5H JH 6D 7D 6H JS KH 3C", &
             "QD 8S 7D 2H 2C 7C JC 2S 5H 8C", &
             "QH 8S 9D TC 2H AD 7C 8D QD 6S", &
             "3S 7C AD 9H 2H 9S JD TS 4C 2D", &
             "3S AS 4H QC 2C 8H 8S 7S TD TC", &
             "JH TH TD 3S 4D 4H 5S 5D QS 2C", &
             "8C QD QH TC 6D 4S 9S 9D 4H QC", &
             "8C JS 9D 6H JD 3H AD 6S TD QC", &
             "KC 8S 3D 7C TD 7D 8D 9H 4S 3S", &
             "6C 4S 3D 9D KD TC KC KS AC 5S", &
             "7C 6S QH 3D JS KD 6H 6D 2D 8C", &
             "JD 2S 5S 4H 8S AC 2D 6S TS 5C", &
             "5H 8C 5S 3C 4S 3D 7C 8D AS 3H", &
             "AS TS 7C 3H AD 7D JC QS 6C 6H", &
             "3S 9S 4C AC QH 5H 5D 9H TS 4H", &
             "6C 5C 7H 7S TD AD JD 5S 2H 2S", &
             "7D 6C KC 3S JD 8D 8S TS QS KH", &
             "8S QS 8D 6C TH AC AH 2C 8H 9S", &
             "7H TD KH QH 8S 3D 4D AH JD AS", &
             "TS 3D 2H JC 2S JH KH 6C QC JS", &
             "KC TH 2D 6H 7S 2S TC 8C 9D QS", &
             "3C 9D 6S KH 8H 6D 5D TH 2C 2H", &
             "6H TC 7D AD 4D 8S TS 9H TD 7S", &
             "JS 6D JD JC 2H AC 6C 3D KH 8D", &
             "KH JD 9S 5D 4H 4C 3H 7S QS 5C", &
             "4H JD 5D 3S 3C 4D KH QH QS 7S", &
             "JD TS 8S QD AH 4C 6H 3S 5S 2C", &
             "QS 3D JD AS 8D TH 7C 6S QC KS", &
             "7S 2H 8C QC 7H AC 6D 2D TH KH", &
             "5S 6C 7H KH 7D AH 8C 5C 7S 3D", &
             "3C KD AD 7D 6C 4D KS 2D 8C 4S", &
             "7C 8D 5S 2D 2S AH AD 2C 9D TD", &
             "3C AD 4S KS JH 7C 5C 8C 9C TH", &
             "AS TD 4D 7C JD 8C QH 3C 5H 9S", &
             "3H 9C 8S 9S 6S QD KS AH 5H JH", &
             "QC 9C 5S 4H 2H TD 7D AS 8C 9D", &
             "8C 2C 9D KD TC 7S 3D KH QC 3C", &
             "4D AS 4C QS 5S 9D 6S JD QH KS", &
             "6D AH 6C 4C 5H TS 9H 7D 3D 5S", &
             "QS JD 7C 8D 9C AC 3S 6S 6C KH", &
             "8H JH 5D 9S 6D AS 6S 3S QC 7H", &
             "QD AD 5C JH 2H AH 4H AS KC 2C", &
             "JH 9C 2C 6H 2D JS 5D 9H KC 6D", &
             "7D 9D KD TH 3H AS 6S QC 6H AD", &
             "JD 4H 7D KC 3H JS 3C TH 3D QS", &
             "4C 3H 8C QD 5H 6H AS 8H AD JD", &
             "TH 8S KD 5D QC 7D JS 5S 5H TS", &
             "7D KC 9D QS 3H 3C 6D TS 7S AH", &
             "7C 4H 7H AH QC AC 4D 5D 6D TH", &
             "3C 4H 2S KD 8H 5H JH TC 6C JD", &
             "4S 8C 3D 4H JS TD 7S JH QS KD", &
             "7C QC KD 4D 7H 6S AD TD TC KH", &
             "5H 9H KC 3H 4D 3D AD 6S QD 6H", &
             "TH 7C 6H TS QH 5S 2C KC TD 6S", &
             "7C 4D 5S JD JH 7D AC KD KH 4H", &
             "7D 6C 8D 8H 5C JH 8S QD TH JD", &
             "8D 7D 6C 7C 9D KD AS 5C QH JH", &
             "9S 2C 8C 3C 4C KS JH 2D 8D 4H", &
             "7S 6C JH KH 8H 3H 9D 2D AH 6D", &
             "4D TC 9C 8D 7H TD KS TH KD 3C", &
             "JD 9H 8D QD AS KD 9D 2C 2S 9C", &
             "8D 3H 5C 7H KS 5H QH 2D 8C 9H", &
             "2D TH 6D QD 6C KC 3H 3S AD 4C", &
             "4H 3H JS 9D 3C TC 5H QH QC JC", &
             "3D 5C 6H 3S 3C JC 5S 7S 2S QH", &
             "AC 5C 8C 4D 5D 4H 2S QD 3C 3H", &
             "2C TD AH 9C KD JS 6S QD 4C QC", &
             "QS 8C 3S 4H TC JS 3H 7C JC AD", &
             "5H 4D 9C KS JC TD 9S TS 8S 9H", &
             "QD TS 7D AS AC 2C TD 6H 8H AH", &
             "6S AD 8C 4S 9H 8D 9D KH 8S 3C", &
             "QS 4D 2D 7S KH JS JC AD 4C 3C", &
             "QS 9S 7H KC TD TH 5H JS AC JH", &
             "6D AC 2S QS 7C AS KS 6S KH 5S", &
             "6D 8H KH 3C QS 2H 5C 9C 9D 6C", &
             "JS 2C 4C 6H 7D JC AC QD TD 3H", &
             "4H QC 8H JD 4C KD KS 5C KC 7S", &
             "6D 2D 3H 2S QD 5S 7H AS TH 6S", &
             "AS 6D 8D 2C 8S TD 8H QD JC AH", &
             "9C 9H 2D TD QH 2H 5C TC 3D 8H", &
             "KC 8S 3D KH 2S TS TC 6S 4D JH", &
             "9H 9D QS AC KC 6H 5D 4D 8D AH", &
             "9S 5C QS 4H 7C 7D 2H 8S AD JS", &
             "3D AC 9S AS 2C 2D 2H 3H JC KH", &
             "7H QH KH JD TC KS 5S 8H 4C 8D", &
             "2H 7H 3S 2S 5H QS 3C AS 9H KD", &
             "AD 3D JD 6H 5S 9C 6D AC 9S 3S", &
             "3D 5D 9C 2D AC 4S 2S AD 6C 6S", &
             "QC 4C 2D 3H 6S KC QH QD 2H JH", &
             "QC 3C 8S 4D 9S 2H 5C 8H QS QD", &
             "6D KD 6S 7H 3S KH 2H 5C JC 6C", &
             "3S 9S TC 6S 8H 2D AD 7S 8S TS", &
             "3C 6H 9C 3H 5C JC 8H QH TD QD", &
             "3C JS QD 5D TD 2C KH 9H TH AS", &
             "9S TC JD 3D 5C 5H AD QH 9H KC", &
             "TC 7H 4H 8H 3H TD 6S AC 7C 2S", &
             "QS 9D 5D 3C JC KS 4D 6C JH 2S", &
             "9S 6S 3C 7H TS 4C KD 6D 3D 9C", &
             "2D 9H AH AC 7H 2S JH 3S 7C QC", &
             "QD 9H 3C 2H AC AS 8S KD 8C KH", &
             "2D 7S TD TH 6D JD 8D 4D 2H 5S", &
             "8S QH KD JD QS JH 4D KC 5H 3S", &
             "3C KH QC 6D 8H 3S AH 7D TD 2D", &
             "5S 9H QH 4S 6S 6C 6D TS TH 7S", &
             "6C 4C 6D QS JS 9C TS 3H 8D 8S", &
             "JS 5C 7S AS 2C AH 2H AD 5S TC", &
             "KD 6C 9C 9D TS 2S JC 4H 2C QD", &
             "QS 9H TC 3H KC KS 4H 3C AD TH", &
             "KH 9C 2H KD 9D TC 7S KC JH 2D", &
             "7C 3S KC AS 8C 5D 9C 9S QH 3H", &
             "2D 8C TD 4C 2H QC 5D TC 2C 7D", &
             "KS 4D 6C QH TD KH 5D 7C AD 8D", &
             "2S 9S 8S 4C 8C 3D 6H QD 7C 7H", &
             "6C 8S QH 5H TS 5C 3C 4S 2S 2H", &
             "8S 6S 2H JC 3S 3H 9D 8C 2S 7H", &
             "QC 2C 8H 9C AC JD 4C 4H 6S 3S", &
             "3H 3S 7D 4C 9S 5H 8H JC 3D TC", &
             "QH 2S 2D 9S KD QD 9H AD 6D 9C", &
             "8D 2D KS 9S JC 4C JD KC 4S TH", &
             "KH TS 6D 4D 5C KD 5H AS 9H AD", &
             "QD JS 7C 6D 5D 5C TH 5H QH QS", &
             "9D QH KH 5H JH 4C 4D TC TH 6C", &
             "KH AS TS 9D KD 9C 7S 4D 8H 5S", &
             "KH AS 2S 7D 9D 4C TS TH AH 7C", &
             "KS 4D AC 8S 9S 8D TH QH 9D 5C", &
             "5D 5C 8C QS TC 4C 3D 3S 2C 8D", &
             "9D KS 2D 3C KC 4S 8C KH 6C JC", &
             "8H AH 6H 7D 7S QD 3C 4C 6C KC", &
             "3H 2C QH 8H AS 7D 4C 8C 4H KC", &
             "QD 5S 4H 2C TD AH JH QH 4C 8S", &
             "3H QS 5S JS 8H 2S 9H 9C 3S 2C", &
             "6H TS 7S JC QD AC TD KC 5S 3H", &
             "QH AS QS 7D JC KC 2C 4C 5C 5S", &
             "QH 3D AS JS 4H 8D 7H JC 2S 9C", &
             "5D 4D 2S 4S 9D 9C 2D QS 8H 7H", &
             "6D 7H 3H JS TS AC 2D JH 7C 8S", &
             "JH 5H KC 3C TC 5S 9H 4C 8H 9D", &
             "8S KC 5H 9H AD KS 9D KH 8D AH", &
             "JC 2H 9H KS 6S 3H QC 5H AH 9C", &
             "5C KH 5S AD 6C JC 9H QC 9C TD", &
             "5S 5D JC QH 2D KS 8H QS 2H TS", &
             "JH 5H 5S AH 7H 3C 8S AS TD KH", &
             "6H 3D JD 2C 4C KC 7S AH 6C JH", &
             "4C KS 9D AD 7S KC 7D 8H 3S 9C", &
             "7H 5C 5H 3C 8H QC 3D KH 6D JC", &
             "2D 4H 5D 7D QC AD AH 9H QH 8H", &
             "KD 8C JS 9D 3S 3C 2H 5D 6D 2S", &
             "8S 6S TS 3C 6H 8D 5S 3H TD 6C", &
             "KS 3D JH 9C 7C 9S QS 5S 4H 6H", &
             "7S 6S TH 4S KC KD 3S JC JH KS", &
             "7C 3C 2S 6D QH 2C 7S 5H 8H AH", &
             "KC 8D QD 6D KH 5C 7H 9D 3D 9C", &
             "6H 2D 8S JS 9S 2S 6D KC 7C TC", &
             "KD 9C JH 7H KC 8S 2S 7S 3D 6H", &
             "4H 9H 2D 4C 8H 7H 5S 8S 2H 8D", &
             "AD 7C 3C 7S 5S 4D 9H 3D JC KH", &
             "5D AS 7D 6D 9C JC 4C QH QS KH", &
             "KD JD 7D 3D QS QC 8S 6D JS QD", &
             "6S 8C 5S QH TH 9H AS AC 2C JD", &
             "QC KS QH 7S 3C 4C 5C KC 5D AH", &
             "6C 4H 9D AH 2C 3H KD 3D TS 5C", &
             "TD 8S QS AS JS 3H KD AC 4H KS", &
             "7D 5D TS 9H 4H 4C 9C 2H 8C QC", &
             "2C 7D 9H 4D KS 4C QH AD KD JS", &
             "QD AD AH KH 9D JS 9H JC KD JD", &
             "8S 3C 4S TS 7S 4D 5C 2S 6H 7C", &
             "JS 7S 5C KD 6D QH 8S TD 2H 6S", &
             "QH 6C TC 6H TD 4C 9D 2H QC 8H", &
             "3D TS 4D 2H 6H 6S 2C 7H 8S 6C", &
             "9H 9D JD JH 3S AH 2C 6S 3H 8S", &
             "2C QS 8C 5S 3H 2S 7D 3C AD 4S", &
             "5C QC QH AS TS 4S 6S 4C 5H JS", &
             "JH 5C TD 4C 6H JS KD KH QS 4H", &
             "TC KH JC 4D 9H 9D 8D KC 3C 8H", &
             "2H TC 8S AD 9S 4H TS 7H 2C 5C", &
             "4H 2S 6C 5S KS AH 9C 7C 8H KD", &
             "TS QH TD QS 3C JH AH 2C 8D 7D", &
             "5D KC 3H 5S AC 4S 7H QS 4C 2H", &
             "3D 7D QC KH JH 6D 6C TD TH KD", &
             "5S 8D TH 6C 9D 7D KH 8C 9S 6D", &
             "JD QS 7S QC 2S QH JC 4S KS 8D", &
             "7S 5S 9S JD KD 9C JC AD 2D 7C", &
             "4S 5H AH JH 9C 5D TD 7C 2D 6S", &
             "KC 6C 7H 6S 9C QD 5S 4H KS TD", &
             "6S 8D KS 2D TH TD 9H JD TS 3S", &
             "KH JS 4H 5D 9D TC TD QC JD TS", &
             "QS QD AC AD 4C 6S 2D AS 3H KC", &
             "4C 7C 3C TD QS 9C KC AS 8D AD", &
             "KC 7H QC 6D 8H 6S 5S AH 7S 8C", &
             "3S AD 9H JC 6D JD AS KH 6S JH", &
             "AD 3D TS KS 7H JH 2D JS QD AC", &
             "9C JD 7C 6D TC 6H 6C JC 3D 3S", &
             "QC KC 3S JC KD 2C 8D AH QS TS", &
             "AS KD 3D JD 8H 7C 8C 5C QD 6C"]
    end subroutine get_euler_data_0054

    !> Data required for problem 0059.
    pure subroutine get_euler_data_0059(euler_data)
        integer, allocatable, intent(out) :: euler_data(:)

        euler_data = &
            [integer :: &
             36, &
             22, &
             80, &
             0, &
             0, &
             4, &
             23, &
             25, &
             19, &
             17, &
             88, &
             4, &
             4, &
             19, &
             21, &
             11, &
             88, &
             22, &
             23, &
             23, &
             29, &
             69, &
             12, &
             24, &
             0, &
             88, &
             25, &
             11, &
             12, &
             2, &
             10, &
             28, &
             5, &
             6, &
             12, &
             25, &
             10, &
             22, &
             80, &
             10, &
             30, &
             80, &
             10, &
             22, &
             21, &
             69, &
             23, &
             22, &
             69, &
             61, &
             5, &
             9, &
             29, &
             2, &
             66, &
             11, &
             80, &
             8, &
             23, &
             3, &
             17, &
             88, &
             19, &
             0, &
             20, &
             21, &
             7, &
             10, &
             17, &
             17, &
             29, &
             20, &
             69, &
             8, &
             17, &
             21, &
             29, &
             2, &
             22, &
             84, &
             80, &
             71, &
             60, &
             21, &
             69, &
             11, &
             5, &
             8, &
             21, &
             25, &
             22, &
             88, &
             3, &
             0, &
             10, &
             25, &
             0, &
             10, &
             5, &
             8, &
             88, &
             2, &
             0, &
             27, &
             25, &
             21, &
             10, &
             31, &
             6, &
             25, &
             2, &
             16, &
             21, &
             82, &
             69, &
             35, &
             63, &
             11, &
             88, &
             4, &
             13, &
             29, &
             80, &
             22, &
             13, &
             29, &
             22, &
             88, &
             31, &
             3, &
             88, &
             3, &
             0, &
             10, &
             25, &
             0, &
             11, &
             80, &
             10, &
             30, &
             80, &
             23, &
             29, &
             19, &
             12, &
             8, &
             2, &
             10, &
             27, &
             17, &
             9, &
             11, &
             45, &
             95, &
             88, &
             57, &
             69, &
             16, &
             17, &
             19, &
             29, &
             80, &
             23, &
             29, &
             19, &
             0, &
             22, &
             4, &
             9, &
             1, &
             80, &
             3, &
             23, &
             5, &
             11, &
             28, &
             92, &
             69, &
             9, &
             5, &
             12, &
             12, &
             21, &
             69, &
             13, &
             30, &
             0, &
             0, &
             0, &
             0, &
             27, &
             4, &
             0, &
             28, &
             28, &
             28, &
             84, &
             80, &
             4, &
             22, &
             80, &
             0, &
             20, &
             21, &
             2, &
             25, &
             30, &
             17, &
             88, &
             21, &
             29, &
             8, &
             2, &
             0, &
             11, &
             3, &
             12, &
             23, &
             30, &
             69, &
             30, &
             31, &
             23, &
             88, &
             4, &
             13, &
             29, &
             80, &
             0, &
             22, &
             4, &
             12, &
             10, &
             21, &
             69, &
             11, &
             5, &
             8, &
             88, &
             31, &
             3, &
             88, &
             4, &
             13, &
             17, &
             3, &
             69, &
             11, &
             21, &
             23, &
             17, &
             21, &
             22, &
             88, &
             65, &
             69, &
             83, &
             80, &
             84, &
             87, &
             68, &
             69, &
             83, &
             80, &
             84, &
             87, &
             73, &
             69, &
             83, &
             80, &
             84, &
             87, &
             65, &
             83, &
             88, &
             91, &
             69, &
             29, &
             4, &
             6, &
             86, &
             92, &
             69, &
             15, &
             24, &
             12, &
             27, &
             24, &
             69, &
             28, &
             21, &
             21, &
             29, &
             30, &
             1, &
             11, &
             80, &
             10, &
             22, &
             80, &
             17, &
             16, &
             21, &
             69, &
             9, &
             5, &
             4, &
             28, &
             2, &
             4, &
             12, &
             5, &
             23, &
             29, &
             80, &
             10, &
             30, &
             80, &
             17, &
             16, &
             21, &
             69, &
             27, &
             25, &
             23, &
             27, &
             28, &
             0, &
             84, &
             80, &
             22, &
             23, &
             80, &
             17, &
             16, &
             17, &
             17, &
             88, &
             25, &
             3, &
             88, &
             4, &
             13, &
             29, &
             80, &
             17, &
             10, &
             5, &
             0, &
             88, &
             3, &
             16, &
             21, &
             80, &
             10, &
             30, &
             80, &
             17, &
             16, &
             25, &
             22, &
             88, &
             3, &
             0, &
             10, &
             25, &
             0, &
             11, &
             80, &
             12, &
             11, &
             80, &
             10, &
             26, &
             4, &
             4, &
             17, &
             30, &
             0, &
             28, &
             92, &
             69, &
             30, &
             2, &
             10, &
             21, &
             80, &
             12, &
             12, &
             80, &
             4, &
             12, &
             80, &
             10, &
             22, &
             19, &
             0, &
             88, &
             4, &
             13, &
             29, &
             80, &
             20, &
             13, &
             17, &
             1, &
             10, &
             17, &
             17, &
             13, &
             2, &
             0, &
             88, &
             31, &
             3, &
             88, &
             4, &
             13, &
             29, &
             80, &
             6, &
             17, &
             2, &
             6, &
             20, &
             21, &
             69, &
             30, &
             31, &
             9, &
             20, &
             31, &
             18, &
             11, &
             94, &
             69, &
             54, &
             17, &
             8, &
             29, &
             28, &
             28, &
             84, &
             80, &
             44, &
             88, &
             24, &
             4, &
             14, &
             21, &
             69, &
             30, &
             31, &
             16, &
             22, &
             20, &
             69, &
             12, &
             24, &
             4, &
             12, &
             80, &
             17, &
             16, &
             21, &
             69, &
             11, &
             5, &
             8, &
             88, &
             31, &
             3, &
             88, &
             4, &
             13, &
             17, &
             3, &
             69, &
             11, &
             21, &
             23, &
             17, &
             21, &
             22, &
             88, &
             25, &
             22, &
             88, &
             17, &
             69, &
             11, &
             25, &
             29, &
             12, &
             24, &
             69, &
             8, &
             17, &
             23, &
             12, &
             80, &
             10, &
             30, &
             80, &
             17, &
             16, &
             21, &
             69, &
             11, &
             1, &
             16, &
             25, &
             2, &
             0, &
             88, &
             31, &
             3, &
             88, &
             4, &
             13, &
             29, &
             80, &
             21, &
             29, &
             2, &
             12, &
             21, &
             21, &
             17, &
             29, &
             2, &
             69, &
             23, &
             22, &
             69, &
             12, &
             24, &
             0, &
             88, &
             19, &
             12, &
             10, &
             19, &
             9, &
             29, &
             80, &
             18, &
             16, &
             31, &
             22, &
             29, &
             80, &
             1, &
             17, &
             17, &
             8, &
             29, &
             4, &
             0, &
             10, &
             80, &
             12, &
             11, &
             80, &
             84, &
             67, &
             80, &
             10, &
             10, &
             80, &
             7, &
             1, &
             80, &
             21, &
             13, &
             4, &
             17, &
             17, &
             30, &
             2, &
             88, &
             4, &
             13, &
             29, &
             80, &
             22, &
             13, &
             29, &
             69, &
             23, &
             22, &
             69, &
             12, &
             24, &
             12, &
             11, &
             80, &
             22, &
             29, &
             2, &
             12, &
             29, &
             3, &
             69, &
             29, &
             1, &
             16, &
             25, &
             28, &
             69, &
             12, &
             31, &
             69, &
             11, &
             92, &
             69, &
             17, &
             4, &
             69, &
             16, &
             17, &
             22, &
             88, &
             4, &
             13, &
             29, &
             80, &
             23, &
             25, &
             4, &
             12, &
             23, &
             80, &
             22, &
             9, &
             2, &
             17, &
             80, &
             70, &
             76, &
             88, &
             29, &
             16, &
             20, &
             4, &
             12, &
             8, &
             28, &
             12, &
             29, &
             20, &
             69, &
             26, &
             9, &
             69, &
             11, &
             80, &
             17, &
             23, &
             80, &
             84, &
             88, &
             31, &
             3, &
             88, &
             4, &
             13, &
             29, &
             80, &
             21, &
             29, &
             2, &
             12, &
             21, &
             21, &
             17, &
             29, &
             2, &
             69, &
             12, &
             31, &
             69, &
             12, &
             24, &
             0, &
             88, &
             20, &
             12, &
             25, &
             29, &
             0, &
             12, &
             21, &
             23, &
             86, &
             80, &
             44, &
             88, &
             7, &
             12, &
             20, &
             28, &
             69, &
             11, &
             31, &
             10, &
             22, &
             80, &
             22, &
             16, &
             31, &
             18, &
             88, &
             4, &
             13, &
             25, &
             4, &
             69, &
             12, &
             24, &
             0, &
             88, &
             3, &
             16, &
             21, &
             80, &
             10, &
             30, &
             80, &
             17, &
             16, &
             25, &
             22, &
             88, &
             3, &
             0, &
             10, &
             25, &
             0, &
             11, &
             80, &
             17, &
             23, &
             80, &
             7, &
             29, &
             80, &
             4, &
             8, &
             0, &
             23, &
             23, &
             8, &
             12, &
             21, &
             17, &
             17, &
             29, &
             28, &
             28, &
             88, &
             65, &
             75, &
             78, &
             68, &
             81, &
             65, &
             67, &
             81, &
             72, &
             70, &
             83, &
             64, &
             68, &
             87, &
             74, &
             70, &
             81, &
             75, &
             70, &
             81, &
             67, &
             80, &
             4, &
             22, &
             20, &
             69, &
             30, &
             2, &
             10, &
             21, &
             80, &
             8, &
             13, &
             28, &
             17, &
             17, &
             0, &
             9, &
             1, &
             25, &
             11, &
             31, &
             80, &
             17, &
             16, &
             25, &
             22, &
             88, &
             30, &
             16, &
             21, &
             18, &
             0, &
             10, &
             80, &
             7, &
             1, &
             80, &
             22, &
             17, &
             8, &
             73, &
             88, &
             17, &
             11, &
             28, &
             80, &
             17, &
             16, &
             21, &
             11, &
             88, &
             4, &
             4, &
             19, &
             25, &
             11, &
             31, &
             80, &
             17, &
             16, &
             21, &
             69, &
             11, &
             1, &
             16, &
             25, &
             2, &
             0, &
             88, &
             2, &
             10, &
             23, &
             4, &
             73, &
             88, &
             4, &
             13, &
             29, &
             80, &
             11, &
             13, &
             29, &
             7, &
             29, &
             2, &
             69, &
             75, &
             94, &
             84, &
             76, &
             65, &
             80, &
             65, &
             66, &
             83, &
             77, &
             67, &
             80, &
             64, &
             73, &
             82, &
             65, &
             67, &
             87, &
             75, &
             72, &
             69, &
             17, &
             3, &
             69, &
             17, &
             30, &
             1, &
             29, &
             21, &
             1, &
             88, &
             0, &
             23, &
             23, &
             20, &
             16, &
             27, &
             21, &
             1, &
             84, &
             80, &
             18, &
             16, &
             25, &
             6, &
             16, &
             80, &
             0, &
             0, &
             0, &
             23, &
             29, &
             3, &
             22, &
             29, &
             3, &
             69, &
             12, &
             24, &
             0, &
             88, &
             0, &
             0, &
             10, &
             25, &
             8, &
             29, &
             4, &
             0, &
             10, &
             80, &
             10, &
             30, &
             80, &
             4, &
             88, &
             19, &
             12, &
             10, &
             19, &
             9, &
             29, &
             80, &
             18, &
             16, &
             31, &
             22, &
             29, &
             80, &
             1, &
             17, &
             17, &
             8, &
             29, &
             4, &
             0, &
             10, &
             80, &
             12, &
             11, &
             80, &
             84, &
             86, &
             80, &
             35, &
             23, &
             28, &
             9, &
             23, &
             7, &
             12, &
             22, &
             23, &
             69, &
             25, &
             23, &
             4, &
             17, &
             30, &
             69, &
             12, &
             24, &
             0, &
             88, &
             3, &
             4, &
             21, &
             21, &
             69, &
             11, &
             4, &
             0, &
             8, &
             3, &
             69, &
             26, &
             9, &
             69, &
             15, &
             24, &
             12, &
             27, &
             24, &
             69, &
             49, &
             80, &
             13, &
             25, &
             20, &
             69, &
             25, &
             2, &
             23, &
             17, &
             6, &
             0, &
             28, &
             80, &
             4, &
             12, &
             80, &
             17, &
             16, &
             25, &
             22, &
             88, &
             3, &
             16, &
             21, &
             92, &
             69, &
             49, &
             80, &
             13, &
             25, &
             6, &
             0, &
             88, &
             20, &
             12, &
             11, &
             19, &
             10, &
             14, &
             21, &
             23, &
             29, &
             20, &
             69, &
             12, &
             24, &
             4, &
             12, &
             80, &
             17, &
             16, &
             21, &
             69, &
             11, &
             5, &
             8, &
             88, &
             31, &
             3, &
             88, &
             4, &
             13, &
             29, &
             80, &
             22, &
             29, &
             2, &
             12, &
             29, &
             3, &
             69, &
             73, &
             80, &
             78, &
             88, &
             65, &
             74, &
             73, &
             70, &
             69, &
             83, &
             80, &
             84, &
             87, &
             72, &
             84, &
             88, &
             91, &
             69, &
             73, &
             95, &
             87, &
             77, &
             70, &
             69, &
             83, &
             80, &
             84, &
             87, &
             70, &
             87, &
             77, &
             80, &
             78, &
             88, &
             21, &
             17, &
             27, &
             94, &
             69, &
             25, &
             28, &
             22, &
             23, &
             80, &
             1, &
             29, &
             0, &
             0, &
             22, &
             20, &
             22, &
             88, &
             31, &
             11, &
             88, &
             4, &
             13, &
             29, &
             80, &
             20, &
             13, &
             17, &
             1, &
             10, &
             17, &
             17, &
             13, &
             2, &
             0, &
             88, &
             31, &
             3, &
             88, &
             4, &
             13, &
             29, &
             80, &
             6, &
             17, &
             2, &
             6, &
             20, &
             21, &
             75, &
             88, &
             62, &
             4, &
             21, &
             21, &
             9, &
             1, &
             92, &
             69, &
             12, &
             24, &
             0, &
             88, &
             3, &
             16, &
             21, &
             80, &
             10, &
             30, &
             80, &
             17, &
             16, &
             25, &
             22, &
             88, &
             29, &
             16, &
             20, &
             4, &
             12, &
             8, &
             28, &
             12, &
             29, &
             20, &
             69, &
             26, &
             9, &
             69, &
             65, &
             64, &
             69, &
             31, &
             25, &
             19, &
             29, &
             3, &
             69, &
             12, &
             24, &
             0, &
             88, &
             18, &
             12, &
             9, &
             5, &
             4, &
             28, &
             2, &
             4, &
             12, &
             21, &
             69, &
             80, &
             22, &
             10, &
             13, &
             2, &
             17, &
             16, &
             80, &
             21, &
             23, &
             7, &
             0, &
             10, &
             89, &
             69, &
             23, &
             22, &
             69, &
             12, &
             24, &
             0, &
             88, &
             19, &
             12, &
             10, &
             19, &
             16, &
             21, &
             22, &
             0, &
             10, &
             21, &
             11, &
             27, &
             21, &
             69, &
             23, &
             22, &
             69, &
             12, &
             24, &
             0, &
             88, &
             0, &
             0, &
             10, &
             25, &
             8, &
             29, &
             4, &
             0, &
             10, &
             80, &
             10, &
             30, &
             80, &
             4, &
             88, &
             19, &
             12, &
             10, &
             19, &
             9, &
             29, &
             80, &
             18, &
             16, &
             31, &
             22, &
             29, &
             80, &
             1, &
             17, &
             17, &
             8, &
             29, &
             4, &
             0, &
             10, &
             80, &
             12, &
             11, &
             80, &
             84, &
             86, &
             80, &
             36, &
             22, &
             20, &
             69, &
             26, &
             9, &
             69, &
             11, &
             25, &
             8, &
             17, &
             28, &
             4, &
             10, &
             80, &
             23, &
             29, &
             17, &
             22, &
             23, &
             30, &
             12, &
             22, &
             23, &
             69, &
             49, &
             80, &
             13, &
             25, &
             6, &
             0, &
             88, &
             28, &
             12, &
             19, &
             21, &
             18, &
             17, &
             3, &
             0, &
             88, &
             18, &
             0, &
             29, &
             30, &
             69, &
             25, &
             18, &
             9, &
             29, &
             80, &
             17, &
             23, &
             80, &
             1, &
             29, &
             4, &
             0, &
             10, &
             29, &
             12, &
             22, &
             21, &
             69, &
             12, &
             24, &
             0, &
             88, &
             3, &
             16, &
             21, &
             3, &
             69, &
             23, &
             22, &
             69, &
             12, &
             24, &
             0, &
             88, &
             3, &
             16, &
             26, &
             3, &
             0, &
             9, &
             5, &
             0, &
             22, &
             4, &
             69, &
             11, &
             21, &
             23, &
             17, &
             21, &
             22, &
             88, &
             25, &
             11, &
             88, &
             7, &
             13, &
             17, &
             19, &
             13, &
             88, &
             4, &
             13, &
             29, &
             80, &
             0, &
             0, &
             0, &
             10, &
             22, &
             21, &
             11, &
             12, &
             3, &
             69, &
             25, &
             2, &
             0, &
             88, &
             21, &
             19, &
             29, &
             30, &
             69, &
             22, &
             5, &
             8, &
             26, &
             21, &
             23, &
             11, &
             94]
    end subroutine get_euler_data_0059
end module euler_data_m
