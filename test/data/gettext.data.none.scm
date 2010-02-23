;; -*- coding: euc-jp -*-
;; encoding-dependent test data for gettext

(define *tests*
  '(("en"
     ("Hello, World!")
     ("Menu|File|Quit" "Quit")
     ("A banana plant in the autumn gale\nI listen to the dripping of rain\nInto a basin at night.\n")
     ("Clouds will separate\nThe two friends, after the migrating\nWild goose's departure\n")
     )
    ("ja"
     ("Hello, World!" "今日は、世界！")
     ("Menu|File|Quit" "終了")
     ("A banana plant in the autumn gale\nI listen to the dripping of rain\nInto a basin at night.\n"
      "芭蕉野分して\nたらいに雨を\n聴く夜かな\n")
     ("Clouds will separate\nThe two friends, after the migrating\nWild goose's departure\n"
      "雲と隔つ\n友かや雁の\n生き別れ\n"))))

(define *plural-tests*
  '(("en"
     ("There is ~D mouse." "There are ~D mice."
      (0 "There are 0 mice.")
      (1 "There is 1 mouse.")
      (2 "There are 2 mice.")))
    ("ja"
     ("There is ~D mouse." "There are ~D mice."
      (0 "0ネズミがあります。")
      (1 "1ネズミがあります。")
      ))))
