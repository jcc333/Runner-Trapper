module TimeConversion where
import Data.Time

minutes n = 60 * n :: DiffTime
hours n = 60 * 60 * n :: DiffTime
seconds n = n :: DiffTime

