module Frustration where
import Euterpea
import Euterpea.Music.Note.Music

addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d = line . map (\n -> n d)

-- Frustration, by Béla Bartók

lowVoice =
  let eighths = addDur en
      section1 = let ostinato = eighths [ d 3, a 3, ef 3, gs 3 ]
                 in timesM 11 ostinato :+: eighths [ d 3, a 3, e 3, bf 3 ] -- bars 1-6
      section2 = let ostinato = timesM 4 (eighths [ f 3, c 4, gf 3, b 3 ])
                     varied = eighths [ f 3, c 4, fs 3, b 3
                                      , gs 3, cs 4, as 3, e 4
                                      , g 3, d 4, af 3, cs 4
                                      , g 3, d 4, af 3, c 4 ]
                 in ostinato :+: varied                                   -- bars 7-10
      section3 = let ostinato1 = timesM 2 (eighths [ g 3, d 4, af 3, cs 4 ])
                     ostinato2 = timesM 2 (eighths [ f 3, ef 4, a 3, c 4 ])
                     varied1 = eighths [ e 3, b 3, bf 3, d 4
                                       , e 3, a 3, af 3, c 4 ]
                     varied2 = timesM 2 (eighths [ ef 3, bf 3, g 3, df 4 ])
                 in ostinato1 :+: ostinato2 :+: varied1 :+: varied2
  in  section1 :+: section2 :+: section3

highVoice = 
  let line1 xs n1 n2 n3 n4 n5 n6 n7 =
         addDur hn xs :+:
         n1 dqn :+: n2 en :+: n3 qn :+: n4 en :+: n5 en :+:
         n6 en  :+: n7 en :+: rest qn :+: rest hn
      s1and2 = let m1 = line1 [ f 4, b 4, d 5, b 4 ] (f 4) (e 4) (a 4) (g 4) (f 4) (e 4) (d 4)
                   m2 = line1 [ a 4, d 5, f 5, d 5 ] (a 4) (gs 4) (e 5) (d 5) (c 5) (b 4) (bf 4)
               in rest (2 * wn) :+: m1 :+: m2 -- bars 1-10
      s3 = let m3 = line [ bf 4 hn, e  5 hn, g 5 wn
                         , g  4 hn, df 5 hn, f 5 wn
                         , a  4 hn, b  4 hn ]
           in m3
  in s1and2 :+: s3

frustration :: Music Pitch
frustration = let t = 69 / 120
              in instrument AcousticGrandPiano
                            (tempo t (lowVoice :=: highVoice))
