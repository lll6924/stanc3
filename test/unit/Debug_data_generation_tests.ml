open Analysis_and_optimization
open Core
open Frontend
open Debug_data_generation

let print_data_prog ast =
  gen_values_json (Ast_to_Mir.gather_declarations ast.Ast.datablock)
  |> Result.ok |> Option.value_exn

let%expect_test "whole program data generation check" =
  let ast =
    Test_utils.typed_ast_of_string_exn
      {|       data {
                  int<lower=7> K;
                  int<lower=1> D;
                  int<lower=0> N;
                  array[N,D] int<lower=0,upper=1> y;
                  array[N] vector[K] x;
                    }
      |}
  in
  let str = print_data_prog ast in
  print_string str;
  [%expect
    {|
      {
        "K": 11,
        "D": 3,
        "N": 1,
        "y": [ [ 1, 1, 0 ] ],
        "x": [
          [
            5.8799711085519224, 4.1465170437036338, 3.543689144366625,
            6.0288479433993629, 3.604405750889411, 4.0759938356540726,
            2.56799363086151, 3.3282621325833865, 2.7103944900448411,
            5.2015419032442969, 4.25312636944623
          ]
        ]
      } |}]

let%expect_test "whole program data generation check" =
  let ast =
    Test_utils.typed_ast_of_string_exn
      {|       data {
                    array[3, 4] int x;
                    array[5, 2, 4] int y;
                    matrix[3, 4] z;
                    vector[3] w;
                    array[4] vector[3] p;
                }
      |}
  in
  let str = print_data_prog ast in
  print_string str;
  [%expect
    {|
      {
        "x": [ [ 2, 3, 5, 6 ], [ 5, 4, 5, 5 ], [ 2, 5, 4, 6 ] ],
        "y": [
          [ [ 3, 2, 2, 3 ], [ 5, 5, 6, 2 ] ],
          [ [ 6, 2, 2, 5 ], [ 5, 4, 6, 4 ] ],
          [ [ 5, 2, 6, 5 ], [ 3, 5, 2, 2 ] ],
          [ [ 3, 4, 2, 5 ], [ 6, 3, 4, 2 ] ],
          [ [ 3, 2, 3, 2 ], [ 5, 3, 2, 3 ] ]
        ],
        "z": [
          [
            4.1949090422787174, 4.1512076186352216, 6.97070615398329,
            3.7293083759369448
          ],
          [
            4.869813970717308, 3.8281495864625956, 2.3295401414257744,
            4.0319385317762162
          ],
          [
            5.7213345511646363, 3.5720962307677091, 3.2425011320285027,
            2.6451502425447266
          ]
        ],
        "w": [ 4.7922353997475788, 4.9461191671001892, 6.8138349711922652 ],
        "p": [
          [ 3.19883563012539, 6.4287706833617158, 6.5986584016153875 ],
          [ 4.6676412390878905, 4.2138065257931459, 5.7759384905058795 ],
          [ 4.0434169569431706, 5.2448759493135153, 2.0095894885098069 ],
          [ 3.8556222147542085, 3.226595023801782, 2.292622453020976 ]
        ]
      } |}]

let%expect_test "whole program data generation check" =
  let ast =
    Test_utils.typed_ast_of_string_exn
      {|       data {
                  int<lower=2, upper=4> K;
                  int<lower=K, upper=K> D;
                  vector[K - 1] x;
                  vector[K * D] y;
                  vector[K ? D : K] z;
                  array[(D + 2 == K) + 3] vector[K ? D : K] w;
                }
      |}
  in
  let str = print_data_prog ast in
  print_string str;
  [%expect
    {|
      {
        "K": 2,
        "D": 2,
        "x": [ 6.8017664359959342 ],
        "y": [
          2.7103944900448411, 5.2015419032442969, 4.25312636944623,
          4.8441784126802627
        ],
        "z": [ 2.56799363086151, 3.3282621325833865 ],
        "w": [
          [ 5.8799711085519224, 4.1465170437036338 ],
          [ 3.543689144366625, 6.0288479433993629 ],
          [ 3.604405750889411, 4.0759938356540726 ]
        ]
      } |}]

let%expect_test "whole program data generation check" =
  let ast =
    Test_utils.typed_ast_of_string_exn
      {|
        data {
          corr_matrix[5] d;
          cov_matrix[4] e;
          cholesky_factor_cov[4] f;
          cholesky_factor_corr[4] g;
          unit_vector[4] h;
          simplex[12] i;
          ordered[2] j;
          positive_ordered[4] k;
          cholesky_factor_cov[5, 4] l;
        }
      |}
  in
  let str = print_data_prog ast in
  print_string str;
  [%expect
    {|
      {
        "d": [
          [
            1.0, 0.30070059205259525, 0.1780531358185751, 0.59291090874043639,
            0.43780890273186973
          ],
          [
            0.30070059205259525, 1.0, 0.45065011470838873, 0.40347551006498511,
            0.7742535367378276
          ],
          [
            0.1780531358185751, 0.45065011470838873, 1.0000000000000002,
            0.47627710137758322, 0.69085242407984848
          ],
          [
            0.59291090874043639, 0.40347551006498511, 0.47627710137758322, 1.0,
            0.72199384900826991
          ],
          [
            0.43780890273186973, 0.7742535367378276, 0.69085242407984848,
            0.72199384900826991, 1.0
          ]
        ],
        "e": [
          [
            3.5187841056791589, 3.0801270965354863, 2.6613103589944083,
            3.0708651420825221
          ],
          [
            3.0801270965354863, 4.1445739878716106, 1.7318254997563731,
            3.284008881887432
          ],
          [
            2.6613103589944083, 1.7318254997563731, 2.94993444818091,
            2.6094058198405388
          ],
          [
            3.0708651420825221, 3.284008881887432, 2.6094058198405388,
            4.2263596365903728
          ]
        ],
        "f": [
          [ 0.691723350374778, 0.0, 0.0, 0.0 ],
          [ 1.9882824615933159, 1.1479255882869233, 0.0, 0.0 ],
          [ 0.877963616911487, 0.86048304745408855, 0.73125983458503818, 0.0 ],
          [
            1.1168941598990316, 1.1784476668400756, 1.925533988476906,
            0.13181605657030976
          ]
        ],
        "g": [
          [ 1.0, 0.0, 0.0, 0.0 ],
          [ 0.8680809071847051, 0.49642274180518731, 0.0, 0.0 ],
          [ 0.84564151733440152, 0.45763410086289852, 0.27470248249292828, 0.0 ],
          [
            0.797502080028714, 0.46262393314442579, 0.38391964675632256,
            0.050746760156257148
          ]
        ],
        "h": [
          0.0064567668878208542, 0.12689825025722787, 0.25917112988209262,
          0.95743692725873575
        ],
        "i": [
          0.1494657066324348, 0.077496253962943171, 0.10532038457686653,
          0.1306129869424564, 0.13770334247073721, 0.011678309728527061,
          0.090541396473787575, 0.043192465951095979, 0.12555826151802352,
          0.10919243895118017, 0.0040762414708886826, 0.015162211321058825
        ],
        "j": [ -0.11142279798014265, 0.35444916424955808 ],
        "k": [
          0.896716387037453, 1.7463321550725821, 1.8890769581764726,
          2.6141237355067388
        ],
        "l": [
          [ 0.38021657248665253, 0.0, 0.0, 0.0 ],
          [ 1.5504623979498982, 1.5303895436871344, 0.0, 0.0 ],
          [ 0.21281572567413909, 0.15087181480686743, 1.7171005741150118, 0.0 ],
          [
            0.62540403375920628, 1.9485914029528901, 1.8510340618361274,
            1.5196891955198353
          ],
          [
            1.8124656303877222, 1.8059981193977444, 1.9574266472261275,
            1.3421609989627226
          ]
        ]
      } |}]

let%expect_test "whole program data generation check" =
  let ast =
    Test_utils.typed_ast_of_string_exn
      {|
        data {
          int<lower=0> N;
          int<lower=0> M;
          int<lower=0, upper=N * M> K;
          array[N] int<upper=N> d_int_1d_ar;
          array[N, M, K] int<upper=N> d_int_3d_ar;
          real<lower=-2.0, upper=2.0> J;
          array[N] real d_real_1d_ar;
          array[N, M, K] real d_real_3d_ar;
          vector[N] d_vec;
          array[N] vector[N] d_1d_vec;
          array[N, M, K] vector[N] d_3d_vec;
          row_vector[N] d_row_vec;
          array[N] row_vector[N] d_1d_row_vec;
          array[N, M, K] row_vector[N] d_3d_row_vec;
          array[4, 5] matrix<lower=0, upper=1>[2, 3] d_ar_mat;
          simplex[N] d_simplex;
          array[N] simplex[N] d_1d_simplex;
          array[N, M, K] simplex[N] d_3d_simplex;
          cholesky_factor_cov[5, 4] d_cfcov_54;
          cholesky_factor_cov[3] d_cfcov_33;
          array[K] cholesky_factor_cov[3] d_cfcov_33_ar;
        }
      |}
  in
  let str = print_data_prog ast in
  print_string str;
  [%expect
    {|
      {
        "N": 3,
        "M": 1,
        "K": 2,
        "d_int_1d_ar": [ 2, 2, -1 ],
        "d_int_3d_ar": [ [ [ -1, 0 ] ], [ [ 2, 3 ] ], [ [ 2, 1 ] ] ],
        "J": -0.937390293933291,
        "d_real_1d_ar": [ 3.604405750889411, 4.0759938356540726, 2.56799363086151 ],
        "d_real_3d_ar": [
          [ [ 2.2826025797056464, 4.521098476935741 ] ],
          [ [ 5.8799711085519224, 4.1465170437036338 ] ],
          [ [ 3.543689144366625, 6.0288479433993629 ] ]
        ],
        "d_vec": [ 5.508750812969728, 3.7482903006745985, 5.3116509882058738 ],
        "d_1d_vec": [
          [ 3.2425011320285027, 2.6451502425447266, 4.5701258402582194 ],
          [ 6.0158175418085342, 3.6598286733521297, 5.3741223139973622 ],
          [ 3.9660375311552749, 2.1600852257225154, 3.7780489469949972 ]
        ],
        "d_3d_vec": [
          [
            [
              [ 5.2448759493135153, 2.0095894885098069, 3.8556222147542085 ],
              [ 3.226595023801782, 2.292622453020976, 4.7922353997475788 ]
            ]
          ],
          [
            [
              [ 4.9461191671001892, 6.8138349711922652, 4.1949090422787174 ],
              [ 4.1512076186352216, 6.97070615398329, 3.7293083759369448 ]
            ]
          ],
          [
            [
              [ 4.869813970717308, 3.8281495864625956, 2.3295401414257744 ],
              [ 4.0319385317762162, 5.7213345511646363, 3.5720962307677091 ]
            ]
          ]
        ],
        "d_row_vec": [ 4.2138065257931459, 5.7759384905058795, 4.0434169569431706 ],
        "d_1d_row_vec": [
          [ 5.5315257534514712, 2.1318346935923507, 2.4903795562578557 ],
          [ 2.0298667610240964, 2.5869872307079236, 3.19883563012539 ],
          [ 6.4287706833617158, 6.5986584016153875, 4.6676412390878905 ]
        ],
        "d_3d_row_vec": [
          [
            [
              [ 5.8259738592178358, 6.29275143528753, 5.7992229887995883 ],
              [ 6.4835819351872654, 6.2480788401756451, 2.7137240155194529 ]
            ]
          ],
          [
            [
              [ 5.6252338866513316, 3.2151318313470769, 4.3293598111485032 ],
              [ 6.8340525891748189, 4.5064008029760334, 5.406294923615655 ]
            ]
          ],
          [
            [
              [ 6.2243138037125068, 6.4536316336809891, 2.3777024486238973 ],
              [ 4.9283096565285138, 3.3969401849295595, 6.0608327679881207 ]
            ]
          ]
        ],
        "d_ar_mat": [
          [
            [
              [ 0.30265828158408009, 0.53857837713598755, 0.99435438254623754 ],
              [ 0.11800610357991351, 0.86493323130460953, 0.70138904116412959 ]
            ],
            [
              [ 0.85689633306737, 0.70234904561486411, 0.34655572443742166 ],
              [ 0.11214741225685448, 0.56198012898460514, 0.80739555544465147 ]
            ],
            [
              [ 0.67800624128286135, 0.187060423355116, 0.65864668749480892 ],
              [ 0.7609047403379835, 0.53296791805267507, 0.71061143479186961 ]
            ],
            [
              [ 0.70107413958330755, 0.27600029727304715, 0.56797857787417749 ],
              [ 0.18822532999887831, 0.90928692941997247, 0.031145119767167891 ]
            ],
            [
              [ 0.4442483267716999, 0.11839458744816142, 0.37368848174295793 ],
              [ 0.42241174496887768, 0.78382782365681281, 0.32234368458918788 ]
            ]
          ],
          [
            [
              [ 0.71651676085087923, 0.62502894666644682, 0.85124674765564845 ],
              [ 0.20495821973392631, 0.73738252703807183, 0.29201824556948469 ]
            ],
            [
              [ 0.23083019388406939, 0.87516028181059635, 0.37399667670291026 ],
              [ 0.97022141344482138, 0.41461589756593475, 0.84919075650439724 ]
            ],
            [
              [ 0.88571739200345234, 0.15572378717971833, 0.612115128160238 ],
              [ 0.94372088555262923, 0.65802867310905055, 0.32648564309712513 ]
            ],
            [
              [ 0.96871518974459825, 0.28083725052478087, 0.760763820464 ],
              [ 0.73727587916715787, 0.28217720706625904, 0.42912560381062059 ]
            ],
            [
              [ 0.10875698401043367, 0.36618179036051896, 0.46310136757192755 ],
              [ 0.90195439170705294, 0.0645661949334827, 0.91994370419748739 ]
            ]
          ],
          [
            [
              [ 0.53312849666186346, 0.42006156138308287, 0.80682474661040526 ],
              [ 0.90178256755944575, 0.1331771098716274, 0.85453819372996653 ]
            ],
            [
              [ 0.70416418983239637, 0.78991889014591155, 0.64874258612805491 ],
              [ 0.91101702970394483, 0.97374370343899963, 0.36659486283528481 ]
            ],
            [
              [ 0.67563924241031859, 0.5801137402153288, 0.90158697893162376 ],
              [ 0.027137609222934244, 0.63969386779910664, 0.77108493814941892 ]
            ],
            [
              [ 0.51405972722094162, 0.49822874401216449, 0.35780805786467229 ],
              [ 0.6497000557737248, 0.41035818879220193, 0.13069440403330407 ]
            ],
            [
              [ 0.43793668913684858, 0.13622771624317104, 0.015030294346858163 ],
              [ 0.89518896165033413, 0.7923933751954082, 0.34067444210810638 ]
            ]
          ],
          [
            [
              [ 0.55973460502049532, 0.44445381850779669, 0.51080157622790656 ],
              [ 0.16357954010812856, 0.17576132217686963, 0.78663225739768683 ]
            ],
            [
              [ 0.29767813861434933, 0.0331482211644266, 0.22407621955437815 ],
              [ 0.79459953975728093, 0.71924027388196421, 0.20559486843854785 ]
            ],
            [
              [ 0.692956694659322, 0.581578803128905, 0.9014260551054567 ],
              [ 0.78390466305359885, 0.975395038570636, 0.57821828544361364 ]
            ],
            [
              [ 0.395832644895654, 0.90623281519386112, 0.90299905969887218 ],
              [ 0.97871332361306373, 0.67108049948136128, 0.31270201687960314 ]
            ],
            [
              [ 0.97429570147644506, 0.9255170309180637, 0.10640786283706954 ],
              [ 0.075435907403433713, 0.77523119897494908, 0.19010828624332626 ]
            ]
          ]
        ],
        "d_simplex": [
          0.46103541990514807, 0.23166736894495787, 0.30729721114989411
        ],
        "d_1d_simplex": [
          [ 0.32369749707698914, 0.41903611877747748, 0.25726638414553332 ],
          [ 0.022643830391066874, 0.50671628568629334, 0.47063988392263983 ],
          [ 0.13209844170749788, 0.77945710856527861, 0.088444449727223429 ]
        ],
        "d_3d_simplex": [
          [
            [
              [ 0.039932594588089045, 0.880151205772848, 0.079916199639062852 ],
              [ 0.33471911233610474, 0.20838925358574761, 0.4568916340781477 ]
            ]
          ],
          [
            [
              [ 0.46120582283716416, 0.17504619363987861, 0.36374798352295723 ],
              [ 0.10950731909322838, 0.15014668749639, 0.74034599341038165 ]
            ]
          ],
          [
            [
              [ 0.18067281549257905, 0.47131712267841813, 0.34801006182900285 ],
              [ 0.69668936436186313, 0.276503877179793, 0.026806758458343793 ]
            ]
          ]
        ],
        "d_cfcov_54": [
          [ 0.61551388622925107, 0.0, 0.0, 0.0 ],
          [ 0.72244097417584985, 0.96619603161228074, 0.0, 0.0 ],
          [ 1.1288383717835269, 1.1262880733299465, 0.48528959541782762, 0.0 ],
          [
            0.077995243596212224, 0.40900327305560347, 1.9760268260453251,
            0.20583822095855378
          ],
          [
            0.041275349703015773, 1.930209633391232, 0.2936087877479292,
            0.37843629716967492
          ]
        ],
        "d_cfcov_33": [
          [ 0.24553267452912819, 0.0, 0.0 ],
          [ 0.494521473356241, 0.8357209625283325, 0.0 ],
          [ 0.49593974277621738, 0.91002206430031829, 1.3323426516081329 ]
        ],
        "d_cfcov_33_ar": [
          [
            [ 0.62851425086711332, 0.0, 0.0 ],
            [ 0.25564525884256228, 1.2808251998906921, 0.0 ],
            [ 1.1444183731928792, 1.5573319981792371, 0.60757110989569574 ]
          ],
          [
            [ 1.5432720473780865, 0.0, 0.0 ],
            [ 1.275024919645803, 0.6402078241901894, 0.0 ],
            [ 1.2238398605980445, 1.0912017876639934, 1.8199435094277936 ]
          ]
        ]
      } |}]

let%expect_test "whole program data generation check" =
  let ast =
    Test_utils.typed_ast_of_string_exn
      {|
        data {
          vector<upper=10>[5] x_vect;
          vector<lower=20.0>[5] x_vect_up;

          vector<lower=x_vect>[5] y_lower_vect;
          vector<upper=x_vect_up>[5] y_upper_vect;
          vector<lower=x_vect, upper=20>[5] y_lower_vect_upper_int;
          vector<lower=x_vect, upper=20.0>[5] y_lower_vect_upper_float;
          vector<lower=1, upper=x_vect_up>[5] y_lower_int_upper_vect;
          vector<lower=1.3, upper=x_vect_up>[5] y_lower_float_upper_vect;
          vector<lower=[1,2,3,4,5]'>[5] y_given_bound;
          vector<lower=0.5, upper=[1,2,3,4,5]'>[5] y_given_bound_and_scalar;
          vector<lower=[1,2,3,4,5]',upper=[2,3,4,5,6]'>[5] y_lu_given_bound;
          vector<lower=x_vect,upper=x_vect_up>[5] y_lu_vector_bound;
          vector<lower=[1,2,3,4,5]',upper=x_vect_up>[5] y_lu_vector_bound_mixed;

          row_vector<upper=10>[5] x_row_vect;
          row_vector<lower=20.0>[5] x_row_vect_up;

          row_vector<lower=x_row_vect>[5] y_row_lower_vect;
          row_vector<upper=x_row_vect_up>[5] y_row_upper_vect;
          row_vector<lower=x_row_vect, upper=20>[5] y_row_lower_vect_upper_int;
          row_vector<lower=x_row_vect, upper=20.0>[5] y_row_lower_vect_upper_float;
          row_vector<lower=1, upper=x_row_vect_up>[5] y_row_lower_int_upper_vect;
          row_vector<lower=1.3, upper=x_row_vect_up>[5] y_row_lower_float_upper_vect;
          row_vector<lower=[1,2,3,4,5]>[5] y_row_given_bound;
          row_vector<lower=0.5, upper=[1,2,3,4,5]>[5] y_row_given_bound_and_scalar;
          row_vector<lower=[1,2,3,4,5],upper=[2,3,4,5,6]>[5] y_row_lu_given_bound;
          row_vector<lower=x_row_vect,upper=x_row_vect_up>[5] y_row_lu_vector_bound;
          row_vector<lower=[1,2,3,4,5],upper=x_row_vect_up>[5] y_row_lu_vector_bound_mixed;

          matrix<upper=2.0>[2,2] upper_matrix;
          matrix<lower=upper_matrix, upper=5>[2,2] lower_upper_matrix;
        }
      |}
  in
  let str = print_data_prog ast in
  print_string str;
  [%expect
    {|
    {
      "x_vect": [
        8.2015419032442978, 7.25312636944623, 7.8441784126802627,
        9.8017664359959333, 7.1815278199399577
      ],
      "x_vect_up": [
        21.604405750889413, 22.075993835654074, 20.567993630861508,
        21.328262132583387, 20.710394490044841
      ],
      "y_lower_vect": [
        12.230389846643661, 8.7968155138128541, 9.9906954563838966,
        13.681737544547856, 9.7026262968757
      ],
      "y_upper_vect": [
        16.887008330595059, 20.387644823859947, 17.316283931536105,
        19.837012945553113, 17.488443437039837
      ],
      "y_lower_vect_upper_int": [
        8.5792936687636541, 12.265292761958673, 16.047224185229535,
        13.187230533411139, 17.476856907913916
      ],
      "y_lower_vect_upper_float": [
        14.266246309179426, 8.89785609233398, 10.864902829276518,
        13.00828734528784, 16.72189250329998
      ],
      "y_lower_int_upper_vect": [
        9.3733771939167347, 2.3890771978580387, 8.1546438928324338,
        12.667666133698285, 7.81707005693119
      ],
      "y_lower_float_upper_vect": [
        21.48544692378394, 10.238695244795489, 9.7582986893893331,
        20.582549733207138, 12.737067049619402
      ],
      "y_given_bound": [
        3.7922353997475788, 2.292622453020976, 4.2265950238017815,
        5.8556222147542085, 5.0095894885098069
      ],
      "y_given_bound_and_scalar": [
        0.82448759493135149, 1.1130250870829512, 2.3879692452529397,
        2.0496645680552019, 2.9008771151791017
      ],
      "y_lu_given_bound": [
        1.9197316803230775, 2.8857541366723432, 3.239767126025078,
        4.1173974461415845, 5.0059733522048191
      ],
      "y_lu_vector_bound": [
        9.51603998848283, 7.6439600075597367, 16.831074637713225,
        19.16320072095121, 10.961331321544691
      ],
      "y_lu_vector_bound_mixed": [
        13.06721606547222, 3.5165504060569628, 18.648274434942191,
        18.640003384204142, 15.702847399887819
      ],
      "x_row_vect": [
        8.6252338866513316, 6.2151318313470769, 7.3293598111485032,
        9.8340525891748189, 7.5064008029760334
      ],
      "x_row_vect_up": [
        24.292751435287528, 23.799222988799588, 24.483581935187267,
        24.248078840175644, 20.713724015519453
      ],
      "y_row_lower_vect": [
        12.451207745869167, 7.1656732625637076, 11.205515806023248,
        10.211232126191987, 8.03844011716138
      ],
      "y_row_upper_vect": [
        23.920336589877845, 23.670701496181813, 21.047092019585282,
        22.603481337582451, 20.607290633584771
      ],
      "y_row_lower_vect_upper_int": [
        18.896636991299772, 18.70743171890166, 12.344812829622761,
        15.712189270972098, 19.692595473643294
      ],
      "y_row_lower_vect_upper_float": [
        17.541966084049413, 18.641171164764664, 14.698335567057759,
        16.878613905060728, 10.075020686212124
      ],
      "y_row_lower_int_upper_vect": [
        17.753084921780715, 19.116252093723773, 6.26211226163225,
        1.7706324590421687, 6.8683446700968274
      ],
      "y_row_lower_float_upper_vect": [
        19.386839965324132, 5.2544931804636361, 5.09235967101705,
        13.021914842963989, 9.928503770154137
      ],
      "y_row_given_bound": [
        3.7986730251024765, 3.703372210540532, 6.9619668759770406,
        8.47594480825167, 5.0751514717342907
      ],
      "y_row_given_bound_and_scalar": [
        0.56811385812158555, 1.1569050337052729, 0.82673601008326014,
        1.9362536607727068, 3.4236502509817615
      ],
      "y_row_lu_given_bound": [
        1.3578080578646723, 2.4982287440121644, 3.5140597272209417,
        4.7710849381494187, 5.6396938677991066
      ],
      "y_row_lu_vector_bound": [
        9.050412855379685, 22.068719455252968, 17.280759768009172,
        19.572734365483463, 12.348137644499662
      ],
      "y_row_lu_vector_bound_mixed": [
        23.68117004588095, 21.859463377110153, 16.93731450392735,
        19.994339965018462, 16.065041740638126
      ],
      "upper_matrix": [
        [ 1.0341237330520263, 1.5089128377972285 ],
        [ -2.334114450641863, 1.2726909686498331 ]
      ],
      "lower_upper_matrix": [
        [ 2.700035909998304, 3.3701108882979232 ],
        [ 4.412857964089933, 1.5133491301453186 ]
      ]
    } |}]

let%expect_test "whole program data generation check" =
  let ast =
    Test_utils.typed_ast_of_string_exn
      {|
      data {
        int<lower = 0> K;                     // players
        int<lower = 0> N;                     // games
        array[N] int<lower=1, upper = K> player1;   // player 1 for game n
        array[N] int<lower=1, upper = K> player0;   // player 0 for game n
        array[N] int<lower = 0, upper = 1> y;       // winner for game n
      }
      |}
  in
  let str = print_data_prog ast in
  print_string str;
  [%expect
    {|
      { "K": 3, "N": 1, "player1": [ 2 ], "player0": [ 1 ], "y": [ 1 ] } |}]

let%expect_test "Complex numbers program" =
  let ast =
    Test_utils.typed_ast_of_string_exn
      {|
      data {
        complex z;
        array[3] complex z_arr;
        complex_vector[2] zvec;
        complex_row_vector[3] zrowvec;
        complex_matrix[2,3] zmat;
      }
      |}
  in
  let str = print_data_prog ast in
  print_string str;
  [%expect
    {|
      {
        "z": [ 6.8017664359959342, 4.1815278199399577 ],
        "z_arr": [
          [ 2.56799363086151, 3.3282621325833865 ],
          [ 2.7103944900448411, 5.2015419032442969 ],
          [ 4.25312636944623, 4.8441784126802627 ]
        ],
        "zvec": [
          [ 3.543689144366625, 6.0288479433993629 ],
          [ 3.604405750889411, 4.0759938356540726 ]
        ],
        "zrowvec": [
          [ 3.7482903006745985, 5.3116509882058738 ],
          [ 2.2826025797056464, 4.521098476935741 ],
          [ 5.8799711085519224, 4.1465170437036338 ]
        ],
        "zmat": [
          [
            [ 5.7213345511646363, 3.5720962307677091 ],
            [ 3.2425011320285027, 2.6451502425447266 ],
            [ 4.5701258402582194, 6.0158175418085342 ]
          ],
          [
            [ 3.6598286733521297, 5.3741223139973622 ],
            [ 3.9660375311552749, 2.1600852257225154 ],
            [ 3.7780489469949972, 5.508750812969728 ]
          ]
        ]
      } |}]

let%expect_test "Tuples program" =
  let ast =
    Test_utils.typed_ast_of_string_exn
      {|
      data {
        tuple(real, int) d;
        tuple(array[2] real, int, matrix[3,3]) complicated;
        tuple(real, tuple(real, real)) n;
        array[3] tuple(real, array[4] tuple(int, int)) nested;
      }
      |}
  in
  let str = print_data_prog ast in
  print_string str;
  [%expect
    {|
      {
        "d": { "1": 4.1815278199399577, "2": 5 },
        "complicated": {
          "1": [ 4.6316923208946168, 3.612729318255341 ],
          "2": 5,
          "3": [
            [ 4.1465170437036338, 3.543689144366625, 6.0288479433993629 ],
            [ 3.604405750889411, 4.0759938356540726, 2.56799363086151 ],
            [ 3.3282621325833865, 2.7103944900448411, 5.2015419032442969 ]
          ]
        },
        "n": {
          "1": 5.8799711085519224,
          "2": { "1": 4.521098476935741, "2": 2.2826025797056464 }
        },
        "nested": [
          {
            "1": 2.6451502425447266,
            "2": [
              { "1": 5, "2": 4 },
              { "1": 5, "2": 6 },
              { "1": 3, "2": 6 },
              { "1": 6, "2": 3 }
            ]
          },
          {
            "1": 3.9660375311552749,
            "2": [
              { "1": 2, "2": 3 },
              { "1": 3, "2": 2 },
              { "1": 5, "2": 5 },
              { "1": 2, "2": 6 }
            ]
          },
          {
            "1": 5.3116509882058738,
            "2": [
              { "1": 5, "2": 2 },
              { "1": 4, "2": 5 },
              { "1": 4, "2": 6 },
              { "1": 2, "2": 5 }
            ]
          }
        ]
      } |}]
