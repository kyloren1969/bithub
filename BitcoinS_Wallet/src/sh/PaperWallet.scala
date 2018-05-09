
package sh

import sh.btc.PrvKey_P2PKH
import sh.btc.PrvKey_P2SH_P2WPKH
import sh.ecc.ECCPrvKey
import sh.util.HashUtil

object PaperWallet {

  /*
   * Takes as input:
   * 1. Option to output ordinary address (-a), segwit address (-s) or private key (-p)
   * 2. Start index
   * 3. sequence of words 
   * 
   * NOTE: DO NOT USE SEGWIT FOR BCH
   * 
   * Outputs 10 addresses or private keys generated from the words starting from index
   * Example: 
   *    java -cp dist\BitcoinS_Wallet.jar a 2022 correct horse battery staple
   *    
   *    will output addresses from 2022 to 2031 using the words correct horse battery staple 
   *    
   * In order to recover the keys, the index and the words are needed
   *    
   */
  def hash(bytes:Array[Byte]) = HashUtil.sha256Bytes2Bytes(bytes)
  def main(args:Array[String]):Unit = {
    if (args.size < 3) {
      println(
"""
Usage java -cp dist/BitcoinS_Wallet.jar sh.PaperWallet <option> <start index> <word1> <word2> ...
<option> can be one of -a, -s or -p denoting ordinary address, segwit address or private key respectively.
<start index> is a BigInteger denoting the starting index for generating the next 10 addresses
<word1>, <word2> ... is an unlimited sequence of words. Use at least 10 for strong security. 
      
Example: java -cp dist/BitcoinS_Wallet.jar sh.PaperWallet -a 2022 correct horse battery staple      
This will output will output addresses from 2022 to 2031 using the words: correct horse battery staple 
""")  
    } else {
      val opt = args(0).toLowerCase
      val isCompressed = true
      val isMainNet = true
      
      val fn = opt match {
        case "-s" => i:BigInt => new PrvKey_P2SH_P2WPKH(i, isCompressed).pubKey.address
        case "-a" => i:BigInt => new PrvKey_P2PKH(new ECCPrvKey(i, isCompressed), isMainNet).pubKey.address
        case "-p" => i:BigInt => //new PrvKey_P2SH_P2WPKH(i, isCompressed).getWIF // should output same as below
           new PrvKey_P2PKH(new ECCPrvKey(i, isCompressed), isMainNet).getWIF
        case any => throw new Exception("Unknown option. Must be -a, -s or -p")
      }
      
      val index = BigInt(args(1))
      val words = args.drop(2).map(_.getBytes("UTF-16"))
 
      val numWords = words.size
      if (numWords < 2) throw new Exception("At least two words are required")
      var tmp = hash(words(1) ++ "ExWs4Hk8ZlG6DsG28A4ksW3T54s6eIYoWrRtkaRwEaYV5NKlRA".getBytes("UTF-16"))
      val numIter = words.size * 193
      
      1 to numIter foreach{i =>
        tmp = hash(words(i % numWords) ++ hash(tmp))
      }
      words.reverse.foreach{w =>
        tmp = hash(hash(w) ++ tmp)
      }
      0 to 9 foreach{i =>
        val int = i + index
        println(int + ": " +fn(BigInt(hash(tmp ++ int.toByteArray)).mod(sh.ecc.Util.n)))
      }
    }
  }
}
