defmodule MiniForth.K do
  @moduledoc """
  About keys.
  """

  @params %{
    p: 0xFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFE_FFFFFC2F,
    a: 0x00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
    b: 0x00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000007,
    G:
      0x04_79BE667E_F9DCBBAC_55A06295_CE870B07_029BFCDB_2DCE28D9_59F2815B_16F81798_483ADA77_26A3C465_5DA4FBFC_0E1108A8_FD17B448_A6855419_9C47D08F_FB10D4B8,
    n: 0xFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFE_BAAEDCE6_AF48A03B_BFD25E8C_D0364141,
    h: 0x01
  }

  def privkey() do
    :crypto.strong_rand_bytes(32)
    |> :binary.decode_unsigned()
    |> rem(@params.n)
  end

  def privkey_to_compressed_pubkey(priv) when is_binary(priv) do
    {publickey, ^priv} = :crypto.generate_key(:ecdh, :secp256k1, priv)
    compress(publickey)
  end

  def privkey_to_compressed_pubkey(priv) when is_integer(priv) do
    priv = :binary.encode_unsigned(priv)
    privkey_to_compressed_pubkey(priv)
  end

  def compress(<<_prefix::size(8), x_coordinate::size(256), y_coordinate::size(256)>>) do
    prefix =
      case rem(y_coordinate, 2) do
        0 -> 0x02
        _ -> 0x03
      end

    <<prefix::size(8), x_coordinate::size(256)>>
  end

  @doc """
  Verfiy with the hash of msg.
  """
  def ecdsa_verify(pk, sig, hash) do
    :crypto.verify(:ecdsa, :sha256, {:digest, hash}, sig, [pk, :secp256k1])
  end
end
