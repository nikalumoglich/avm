class CalculatePriceResponseDto {
  CalculatePriceResponseDto(this.value);

  double value;

  CalculatePriceResponseDto.fromJson(Map<String, dynamic> json)
      : value = json['value'].toDouble();

  Map<String, dynamic> toJson() => {
    'value': value,
  };
}