defmodule MiniForth.K do
  @moduledoc """
  About keys.
  """

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
