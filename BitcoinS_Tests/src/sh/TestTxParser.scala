
package sh

import sh.btc._
import sh.ecc.Util._
import scala.collection.JavaConversions._
import sh.util._
import sh.btc.BitcoinUtil._

object TxParserTest extends App {
  import TxParserTestVectors._
  isMainNet = true
  Seq(
    coinBaseTx, 
    dataTx1, 
    coinbaseBurnTx, 
    randomTx,
    segWitTx1,
    segWitTx2,
    segWitTx3,
    segWitTx4,
    segWitTx5,
    segWitTx6,
    dataTx2,
    dataTx3,
    strangeTx1,
    strangeTx2,
    strangeTx3,
    strangeTx4,
    strangeTx5,
    strangeTx6,
    strangeTx7,
    strangeTx8
  ).foreach{
    case (hex, txid, hash, vSize) => // hash is segwitTxID
      new TxParserTest(hex, txid, hash, vSize)
  }
  println("All tx parser tests passed!")
}
class TxParserTest(hex:String, txid:String, hash:String, vSize:Int) {
  val size = hex.size/2 
  val bytes = hex.decodeHex

  val tx = new TxParserSegWit(bytes).getSegWitTx
  val txBytes = tx.serialize
  
  assert(tx.vSize == vSize, s"TxVSize. Found: ${tx.vSize}. Expected: ${vSize}")
  assert(txBytes.size == size, s"TxBytesSize. Found: ${txBytes.size}. Expected: ${size}")
  assert(tx.size == size, s"TxSize. Found: ${tx.size}. Expected ${size}")
  assert(tx.txid == txid, s"TxId. Found ${tx.txid}. Expected ${txid}")
  assert(tx.segWitTxHash == hash, s"TxHash. Found ${tx.segWitTxHash}. Expected ${hash}")
  assert(txBytes.size == bytes.size, s"TxBytesSize. Found ${txBytes.size}. Expected ${bytes.size}")
  if (tx.vSize == tx.size) assert(!tx.isSegWit)
  if (!tx.isSegWit) assert(tx.vSize == tx.size)
  if (tx.vSize != tx.size) assert(tx.isSegWit)
  if (tx.isSegWit) assert(tx.vSize != tx.size)
  (txBytes zip bytes).zipWithIndex.foreach{
    case ((l, r), i) => assert(l == r, s"Left ($l) != Right ($r) at index $i")
  }

  val newBytes = createSegWitTxRawAdvanced(tx.version, tx.ins zip tx.witnesses, tx.outs, tx.lockTime)
  assert(newBytes.size == bytes.size)
  (newBytes zip bytes).zipWithIndex.foreach{
    case ((l, r), i) => assert(l == r, s"Left ($l) != Right ($r) at index $i")
  }
  
  val newTx = new TxParserSegWit(newBytes).getSegWitTx
  val newTxBytes = newTx.serialize
  assert(newTx.txid == tx.txid, s"Computed txid ${newTx.txid} != ${tx.txid}")
  assert(newTx.segWitTxHash == tx.segWitTxHash)
  assert(newTx.size == tx.size)
  assert(newTx.version == tx.version)
  assert(newTx.vSize == tx.vSize)
  println(s"Tx test passed for $txid "+(if (newTx.isSegWit) "(SegWit)" else ""))
}
object TxParserTestVectors {
  val coinBaseTx = (// coinbase tx
    "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff1b0356770506cbcde1b6e3fb084e8b873474fe192306457336e3ffceffffffff0121430696000000001976a9147f8723c3a5e64d6e1d47511863aca2f146b0a85588ac00000000",
    "9a223b09449cdd383206db29fe7f5ea31e0154d9fb30eabafe36eb8a9a33d22f", 
    "9a223b09449cdd383206db29fe7f5ea31e0154d9fb30eabafe36eb8a9a33d22f", 
    112
  )
  
  val dataTx1 = (// contains data (OP_Return)
    "01000000013b27df80c94895f426f2a83de8394f385dbf2d3ba3bd1945d88978397086242a020000008a4730440220099517d79515f27783e5e39981986ba87d54f29c3c54ca8ba18090172985b125022073700a3860e40ed534358bbb8a624eacb2804a36f4b3ab92e0d720c50a144549014104fc49b907b56beec9b810692456297db5a75521ea3614feb2bf77730e8517c254b114013f363b4f643e2f9cd0a7233ae240f5f8904052476be2d6d6fe66325456ffffffff010000000000000000246a225331a04d8f666b8dc3a331402438cf32057c4176950cef92cac63a4b3612d7ce91a500000000",
    "b9a36957d981d927307f7af07fe1f33823a73278fa5657cd247911b04d141ee5",
    "b9a36957d981d927307f7af07fe1f33823a73278fa5657cd247911b04d141ee5",
    234
  )
  
  val randomTx = (// had an error initially
    "0100000001507b2a307643dfde8780ddd5ab1e959effd6cd886e4c63b076e36e4eea7e1ee507000000fdff000048304502206d2179d8918c479268cbd45a38fe730b1303eb9cea17c1960f5ed983cf6524120221009b6812909567c064681cd6afc629aeccebcfa67eaa1fbdbd2b221f5510b269d901493046022100a869fd885752c1296c9a7124b3e8064a4dda4bb82dfe516aaebc1b9b2e59dee6022100ec5ebda8cde9c9e1a2de28a8ae04a4b85dcfb0549994b6248941301210c66a98014c6952210233137e14a3de838740face22048958afa84860ddf2c98b49b217a3d3ad1e01bc210346eaacb853dc05f85156696996d32d19068d99009d3a234b89ce57b648c1e64a2103f6ac742f91d46dacf2d1fe1fb42734b0faff9b4f7abbad762fa30f6a375f603a53aeffffffff0f00e1f505000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488acd07cac29000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488aca08b87e7000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac404ae314000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac2048531c000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488acc044334f000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac50231923000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488acb0d71c24000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac309bb119000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac30d64c02000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac5013db11000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488acc07e4e3b000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac90cbb81d000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac70a38f7e010000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ace0f700c00700000017a914f4c25c8efe5e0443d22d5cb3a034a5088293d9698700000000",
    "a42dc25ead3237a07c5bde358febae04f84b5b357f47c4df1e69e108b46a02a6",
    "a42dc25ead3237a07c5bde358febae04f84b5b357f47c4df1e69e108b46a02a6",
    816
  )
  
  // https://blockchain.info/tx/9bf8853b3a823bbfa1e54017ae11a9e1f4d08a854dcce9f24e08114f2c921182
  // coin base with burn, no reward, size = 119. JSON below:
  /*
{
	"result": {
		"txid": "9bf8853b3a823bbfa1e54017ae11a9e1f4d08a854dcce9f24e08114f2c921182",
		"hash": "9bf8853b3a823bbfa1e54017ae11a9e1f4d08a854dcce9f24e08114f2c921182",
		"version": 1,
		"size": 119,
		"vsize": 119,
		"locktime": 0,
		"vin": [
			{
				"coinbase": "03dea707055a478cb801b80100006ea50000",
				"sequence": 4294967295
			}
		],
		"vout": [
			{
				"value": 0.00000000,
				"n": 0,
				"scriptPubKey": {
					"asm": "2 3 [error]",
					"hex": "52534b424c4f434b3addbf517adf8ffd4bca7751505b39c9013a0d1fd479fc4e901b39dd57b347c624",
					"type": "nonstandard"
				}
			}
		],
		"hex": "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff1203dea707055a478cb801b80100006ea50000ffffffff0100000000000000002952534b424c4f434b3addbf517adf8ffd4bca7751505b39c9013a0d1fd479fc4e901b39dd57b347c62400000000",
		"blockhash": "0000000000000000004b27f9ee7ba33d6f048f684aaeb0eea4befd80f1701126",
		"confirmations": 178,
		"time": 1514638520,
		"blocktime": 1514638520
	},
	"error": null,
	"id": null
}
*/
  val coinbaseBurnTx = (
    "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff1203dea707055a478cb801b80100006ea50000ffffffff0100000000000000002952534b424c4f434b3addbf517adf8ffd4bca7751505b39c9013a0d1fd479fc4e901b39dd57b347c62400000000",
    "9bf8853b3a823bbfa1e54017ae11a9e1f4d08a854dcce9f24e08114f2c921182",
    "9bf8853b3a823bbfa1e54017ae11a9e1f4d08a854dcce9f24e08114f2c921182",
    119 // vSize
  )
  val segWitTx1 = ( // default implementers guide P2SH-P2WPKH (https://bitcoin.stackexchange.com/a/60894/2075)
    "01000000000101db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a5477010000001716001479091972186c449eb1ded22b78e40d009bdf0089feffffff02b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac02473044022047ac8e878352d3ebbde1c94ce3a10d057c24175747116f8288e5d794d12d482f0220217f36a485cae903c713331d877c1f64677e3622ad4010726870540656fe9dcb012103ad1d8e89212f0b92c74d23bb710c00662ad1470198ac48c43f7d6f93a2a2687392040000",
    "ef48d9d0f595052e0f8cdcf825f7a5e50b6a388a81f206f3f4846e5ecd7a0c23",
    "680f483b2bf6c5dcbf111e69e885ba248a41a5e92070cfb0afec3cfc49a9fabb",
    170 // vSize (Size is 251)
  )
  val segWitTx2 = ( // unsigned version of above (https://bitcoin.stackexchange.com/a/60894/2075)
    "0100000001db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a54770100000000feffffff02b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac92040000",
    "321a59707939041eeb0d524f34432c0c46ca3920f0964e6c23697581f176b6c0",
    "321a59707939041eeb0d524f34432c0c46ca3920f0964e6c23697581f176b6c0",
    119 // vSize (size is also 119 because its not signed yet, so size is same as a classic tx)
  )
  val segWitTx3 = ( // should not understand this tx as uses Native P2WPKH input
    "01000000000101db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a5477010000001716001479091972186c449eb1ded22b78e40d009bdf0089feffffff02b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac02473044022047ac8e878352d3ebbde1c94ce3a10d057c24175747116f8288e5d794d12d482f0220217f36a485cae903c713331d877c1f64677e3622ad4010726870540656fe9dcb012103ad1d8e89212f0b92c74d23bb710c00662ad1470198ac48c43f7d6f93a2a2687392040000",
    "ef48d9d0f595052e0f8cdcf825f7a5e50b6a388a81f206f3f4846e5ecd7a0c23",
    "680f483b2bf6c5dcbf111e69e885ba248a41a5e92070cfb0afec3cfc49a9fabb",
    170 // vSize (Size is 251)
  )
  val segWitTx4 = ( // https://bitcoin.stackexchange.com/a/60895/2075
    "0200000000010140d43a99926d43eb0e619bf0b3d83b4a31f60c176beecfb9d35bf45e54d0f7420100000017160014a4b4ca48de0b3fffc15404a1acdc8dbaae226955ffffffff0100e1f5050000000017a9144a1154d50b03292b3024370901711946cb7cccc387024830450221008604ef8f6d8afa892dee0f31259b6ce02dd70c545cfcfed8148179971876c54a022076d771d6e91bed212783c9b06e0de600fab2d518fad6f15a2b191d7fbd262a3e0121039d25ab79f41f75ceaf882411fd41fa670a4c672c23ffaf0e361a969cde0692e800000000",
    "c586389e5e4b3acb9d6c8be1c19ae8ab2795397633176f5a6442a261bbdefc3a",
    "b759d39a8596b70b3a46700b83e1edb247e17ba58df305421864fe7a9ac142ea",
    134 // vSize (Size is 216)
  )
  val segWitTx5 = ( // coinbase of block 00000000000000000013460c16ffa09553e9739aebbd467d7bf34284f2dd5134
    "010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2403e0a707174d696e656420627920416e74506f6f6c6e06205a478f47b1080000433c8ee3ffffffff02807c814a000000001976a914eb181d60731cc7521db83a8f93beba8da692a22988ac0000000000000000266a24aa21a9ede2f61c3f71d1defd3fa999dfa36953755c690689799962b48bebd836974e8cf90120000000000000000000000000000000000000000000000000000000000000000000000000",
    "d236a5a45ddcc90e5f04bd27375f0fe88c4924179b6992a4ce7004393a169f91",
    "c466e6453951fc049b72022073cdeb5e6dc5979917b38bc0380f61438509fe1c",
    177 // vSize (Size is 204)
  )
  val segWitTx6 = ( // bech32 addresses txid 1263c4d5f6eaebfef32702bf72f07b5bbcd6bcafb23a111b5acd6293c58462a3
    "01000000000101f4a762db5b529818b873e6e1f2fa85f803bb9e859db385c2886c58145b8e1cd90300000000ffffffff03002d3101000000001976a9143bd56589f79ea155f3446c9c2689a0a94adde34a88acc0d40100000000001976a9146f7cf85c98494c4d272e099278a36aaa609abeac88ac83e4750000000000220020701a8d401c84fb13e6baf169d59684e17abd9fa216c8cc5b9fc63d622ff8c58d04004730440220720b5303d83bccaa66386c235b5b86f145177b7a95ec5da342671625ba0f9e910220710be4f13e869fe49c30e6d655ea3f8f2722cc09c14f9d3171edd9fa22f030fa01483045022100d6e783056129fdf10014fea013a49b5f45d55a240f40156fc703b0372dee0d9e02201b3649a7434145c70a3dd01a8a174c4de5efe664fc4f1fdda9972897a6d4e023016952210375e00eb72e29da82b89367947f29ef34afb75e8654f6ea368e0acdfd92976b7c2103a1b26313f430c4b15bb1fdce663207659d8cac749a0e53d70eff01874496feff2103c96d495bfdd5ba4145e3e046fee45e84a8a48ad05bd8dbb395c011a32cf9f88053ae00000000",
    "1263c4d5f6eaebfef32702bf72f07b5bbcd6bcafb23a111b5acd6293c58462a3",
    "38f6b065e5ce871cb33680ae015cea8c05422227366d53e3f183874964a2c6a2",
    226 // vSize (Size is 417)
  )
  val dataTx2 = ( // f3e965def304595be4cb4c56156dd9720f97e9833c4d010263268e032002c48f
    "0100000001d85e9cce452525e798d8538689570a31723cc67957a6015068590c1289181835010000006b483045022100d3fdcec2c79ae4315898fdf2208a71ad64e3be2a908e1e2204df5efa1e41d489022061fca90033cc0e63c3972f09e99ce3647fa830cad9b268158eaf6f54727326bd012102523859a1629f9fd928b2fa07665f3724016cb72d27bed0cfcc5cc83d51c2fff6ffffffff020000000000000000356a331bb279247b9ec41dc592097e5e49e3d42d305ef30f0580cac0f84f5ad6414c6fa3f845494076e03e616eac13bd6012b4dc9cbfe4c40e00000000001976a9146fe36e6d8c9738a7ed460215f0b3669b57bd0aaa88ac00000000",
    "f3e965def304595be4cb4c56156dd9720f97e9833c4d010263268e032002c48f",
    "f3e965def304595be4cb4c56156dd9720f97e9833c4d010263268e032002c48f",
    254 // vSize (Size is also 254)
  )
  val dataTx3 = ( // d545908e187568f5dc283629088056dd3ecd748302b271e9e7939942ef82fe30
    "0100000002c434be3110a64ec4d82eb08695879b7ac139195702f885e0b0083dbf3c0fb79400000000fd68010048304502210096bb33dfc1e6c5b269a12170fa73e60a3337a1a1b41e87f546964f635401a075022079b8680ab1ad5fc7380b92e96aedd13cf0d7e1e474d63f3ff41b203510f963510147304402204b80dfd6e2bb3b353d6ea47d8484d18fa3d7093b6978e903a33839e5c292aaef02206e2c7ab7a4fb0df1d55854beb398377088f510497b86b568cdf5bfe30c8692f701483045022100d630f0503053c2b2614997b2285a9d6652d36eaddb89d9c7f24df854d9b8e0980220260985598b8caaba90a65fa6d0245c7979bfcbda7c15ac5495723e6b1be75c41014c8b5321026ab024fe8e1a82bc7d1e793a9269080b128690530d8f603e676d57161171628c210205aa7c07450751339ce344dad1faa3df82e52cd655a5657dc99611ab6663ab082102ae62f31a84aa0137235c52024cd410108ecc1ef66669b8a9a2853c08c370835b2102d9e879e7c56f4f3c53fd39a07cd5736ad4e205989410b71622cc7c17c7d92a0554aeffffffff8d722ee47babf0eec85a0fa9232c08fcac6ab080f7a3158340b2bd3bc91472ec02000000fd6701004730440220195846119c09e8197ee84f6adad02eb1ae64522806d40d1da91e901a2036793602207809143d7d938be69f5ac46a93719a8bd50f6f84b8d25aeb5a0a37a21e870392014730440220131c18441d2d96e4197512a95273431046e41980686c089897faf4d64cdbc74002207b45cb48e6f84028f6d1a6fffd2b09051ebe6c7d3ca0d10ee7c4a8bb0ef6310e014830450221009f20b6ece5fc72ada32fe104ca78f15b99f8f016813b548f1d508673a0cbfd660220175f445f6b67b70a238ce5fdd61f6a65d842c680fc8fa93010c28ae69a3546d5014c8b5321026ab024fe8e1a82bc7d1e793a9269080b128690530d8f603e676d57161171628c210205aa7c07450751339ce344dad1faa3df82e52cd655a5657dc99611ab6663ab082102ae62f31a84aa0137235c52024cd410108ecc1ef66669b8a9a2853c08c370835b2102d9e879e7c56f4f3c53fd39a07cd5736ad4e205989410b71622cc7c17c7d92a0554aeffffffff0354150000000000001976a9142fcb8986490ec18f8f199bcbcc4836e28394573488ac0000000000000000166a146f6d6e69000000000000001f00000004a817c800400d03000000000017a914aac75bdbbf77ccb608169ebb0709ae3f69cc44cd8700000000",
    "d545908e187568f5dc283629088056dd3ecd748302b271e9e7939942ef82fe30",
    "d545908e187568f5dc283629088056dd3ecd748302b271e9e7939942ef82fe30",
    912 // vSize (Size is also 931)
  )
  //   more "strange transactions" from blockchain.info
  val strangeTx1 = (
    "0100000001b66bd6278fc7413ab6de2bf0212841922663caef05863520e3d0ef5c317c0f6f490000008a473044022013f0ac03f8e4cf29274face9aee09512b6c08dccfc83b2f22f1774d8b547c716022046445239955e1cbfeb1f368d3eb77a6508da00173e722b85586f906945a2bfee014104dab8b2cd8e167637fd1736422f12497a8aba247ac8020cdcd6e198a83fd3291870356ab3a8dbe790e8afabce817b28c73db73cc6b5c3bab2aabf191a32699f67ffffffff020000000000000000306a2e1f0c8b1cffcc40b2544e36337260868dceccd043028320f00a7c339cd523bd8df01a26b9eb868b3d99197c6c783a36150000000000001976a9149d33d96b9e10f7e9920731a5fd996af98a9928e788ac00000000", // hex
    "f420d053e30858bfa701da6298f8e048201b8152f6c87ad2a8ac02678d740072", // txid
    "f420d053e30858bfa701da6298f8e048201b8152f6c87ad2a8ac02678d740072", // hash
    280 // vSize
  )
  val strangeTx2 = (
    "0100000001c4e5750db7abe1d7b39de33bc4007491cc0d3d9ba7ffc3b89ecd08d49fe62b30000000006a473044022009d11e7e7b3fe4ad226ada4d818b6e8ab1cb222030ae7e237023d5681708c9cf02205ea75ffcb026c957ea6ef7e1af938e96494e701891d1bbae9d0f0dfe033047650121027df8ef93c504fbd590908f3c1d3f4959428572bda3f5e08768c0e29e2a328090feffffff030000000000000000166a146f6d6e69000000000000001f00000010229a150022020000000000001976a91488d924f51033b74a895863a5fb57fd545529df7d88ac3d0b0500000000001976a9142e7b607d17a7dbc8dca02a924094c2bbfed92c9b88aca7a80700", // hex
    "a8e90046ff8a5fce6e9a087add1e7e31a4105c1b173d0cb7e57510588d1640f8", // txid
    "a8e90046ff8a5fce6e9a087add1e7e31a4105c1b173d0cb7e57510588d1640f8", // hash
    256 // vSize
  )
  val strangeTx3 = ( // segwit
    "010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff4503a6a807fabe6d6d0b2e48cd6f229c9514912a106a90f78af2aa0577de7f1d996b6157f1c4618eab0100000000000000216503017607cb001100d7ffb0122f736c7573682f0000000002c84e316e000000001976a9147c154ed1dc59609e3d26abb2df2ea3d587cd8c4188ac0000000000000000266a24aa21a9edeafffdb6c993630e5961b4f8a048e5a3d031ab39a4b9d36912d1674fb8842dbd0120000000000000000000000000000000000000000000000000000000000000000000000000", // hex
    "8d8f7e8e642d2380cbea4237dc6d3272f6f426ff7b91f53fa736af80dfb4ef98", // txid
    "218f0cdedd70011ebb9c7885f7ba8c2a05b0ef76df61ee88cf222fcb363ccda5", // hash
    210 // vSize
  )
  val strangeTx4 = (
    "0100000001b66bd6278fc7413ab6de2bf0212841922663caef05863520e3d0ef5c317c0f6f4a0000008b483045022100ae463798ce47e49be42f21277fedbf552a504290b7d8b208f9d709a5142aa82c0220408047e14a385f659b715cfab0ac7f2c7cd6064fd895030744973a7f41ba40fb014104dab8b2cd8e167637fd1736422f12497a8aba247ac8020cdcd6e198a83fd3291870356ab3a8dbe790e8afabce817b28c73db73cc6b5c3bab2aabf191a32699f67ffffffff020000000000000000306a2e1f0c8b1cffcc40b2544e36337260868dceccd047b48ae60a567ce571c80998051916906f7f0b15911dac1126775f36150000000000001976a9149d33d96b9e10f7e9920731a5fd996af98a9928e788ac00000000", // hex
    "49326de67066f3d785d4a493669e8eed0b0450374844e21fbcce16323bf35582", // txid
    "49326de67066f3d785d4a493669e8eed0b0450374844e21fbcce16323bf35582", // hash
    281 // vSize
  )
  val strangeTx5 = (
    "01000000015b5cc83e7844abdce289917e6e4a6391779112946b81e2e95d401fb7a14f521a260200008b4830450221009630b9cb637a304d0b8856852f582a75449fe4137e6b855cf425354961fe08290220589cddfcdfbabe085e17b6c82152d05e0a57538b1c9765e09a92f5d0b896a1460141043fd05d2490b2b8496da3b8498f1aee08634c43eef40aa58b66e36271632571d6dbda0ec7fb6fcec09409597e38e3b968648ed10ad6402647bb6143f0f9f09b3fffffffff020000000000000000306a2ef24683194a9d98d61c471c8c67d754c3cfe6c42fef914146e8175baaf2dc0b9938d99aaa1da2b7fee0ebde4d1f7236150000000000001976a9146fe066d1be6359dba98e7f00f3f45707e0bd07ad88ac00000000", // hex
    "9b195c66ceac7b70f9d5d8a47b0c1e66e85a010bf339d914a3a20aff96be11f9", // txid
    "9b195c66ceac7b70f9d5d8a47b0c1e66e85a010bf339d914a3a20aff96be11f9", // hash
    281 // vSize
  )
  val strangeTx6 = (
    "010000000140105a72328580fd0bdc7ece06c52db00eb1c5c54f88c43b4785cb1f24002f40000000006a47304402207a3ee9114220f03ad6bba0cb2469457f76b69f31cf5101db05fb94f5e170b6790220134b6732daa544e32747eed027f7617a187a813fa939831f536027fa8091af0f012102f9ad2670f6d40f1d8d80bcba59b8ddaacf955aca877be1ee703d7dce268fb572feffffff030000000000000000166a146f6d6e69000000000000001f0000001be365368122020000000000001976a91488d924f51033b74a895863a5fb57fd545529df7d88accf0d0500000000001976a914a3214900a800e102373177098b1fcd9e5a2a9b3e88aca7a80700", // hex
    "580c97fb557c6613c32cc9af4bb6cf257d35b2daf34035db39813f5730f81b40", // txid
    "580c97fb557c6613c32cc9af4bb6cf257d35b2daf34035db39813f5730f81b40", // hash
    256 // vSize
  )
  val strangeTx7 = (
    "010000000136b7159c9e037872d88b0c5959bc2719affa8e53139874b06668889f0bf556653c0000008b48304502210084ad540a1c9e9ea065c1fb549327d501c11a7e11378b41e8ca089857832d57d002202d9ecaa334adce78c043e83568688845c16e4306b4ffaa3998be20825822ebfd014104fcf07bb1222f7925f2b7cc15183a40443c578e62ea17100aa3b44ba66905c95d4980aec4cd2f6eb426d1b1ec45d76724f26901099416b9265b76ba67c8b0b73dffffffff0388080000000000001976a914fa0692278afe508514b5ffee8fe5e97732ce066988ac0000000000000000166a146f6d6e69000000000000001f000000174876e80022020000000000001976a914e0186b5d311417f1cfea1e9a71e3de423ce7a89b88ac00000000", // hex
    "29ab1329739061333eba13165509cb2fc60786d45f4192c46ebec773da9ddf49", // txid
    "29ab1329739061333eba13165509cb2fc60786d45f4192c46ebec773da9ddf49", // hash
    289 // vSize
  )
  val strangeTx8 = (
    "01000000013d29222cd1c8c1a02348b373dd2f8688a181313c4e089b646169570a274a6c75010000006a47304402203515b767721c89e740c33a18f20116d00012ac1de0231e1a010d311a7f73954b02200b11b104630262f016545b779448fb67d5a8189cc23b13ff8ac74ffbe167c99f012102b7042dc82d0ecbadc24910515789f34674fe32a285676c7f7b0c64f750b10e36feffffff030000000000000000166a146f6d6e69000000000000001f00000005c8965d7022020000000000001976a914b154aa9145caa340fd9a2292b16bf201bc87befc88ac26a3c901000000001976a91446a8a27a7f51f84dc75ae07019df18ca39cecd2288aca5a80700", // hex
    "42de7d41444157f3949990a144751fc6eca3d0f7f17cd10fc661cb2bf724bd63", // txid
    "42de7d41444157f3949990a144751fc6eca3d0f7f17cd10fc661cb2bf724bd63", // hash
    256 // vSize
  )
}
