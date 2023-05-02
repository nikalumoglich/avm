class CalculatePriceRequestDto {
  CalculatePriceRequestDto(this.productId, this.dimensionValues);

  int productId;
  List<DimensionValueDto> dimensionValues;

  CalculatePriceRequestDto.fromJson(Map<String, dynamic> json)
      : productId = json['productId'],
        dimensionValues = List<DimensionValueDto>.from(json['dimensionValues'].map((model)=> DimensionValueDto.fromJson(model)));

  Map<String, dynamic> toJson() => {
    'productId': productId,
    'dimensionValues': dimensionValues.map((model) => model.toJson()).toList()
  };
}

class DimensionValueDto {
  DimensionValueDto(this.dimensionId, this.value);

  int dimensionId;
  double value;

  DimensionValueDto.fromJson(Map<String, dynamic> json)
      : dimensionId = json['dimensionId'],
        value = json['value'];

  Map<String, dynamic> toJson() => {
    'dimensionId': dimensionId,
    'value': value,
  };
}