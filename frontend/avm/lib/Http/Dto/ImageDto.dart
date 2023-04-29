class ImageDto {
  ImageDto(this.id, this.url);

  int id;
  String url;

  ImageDto.fromJson(Map<String, dynamic> json)
      : id = json['imageId'],
        url = json['url'];

  Map<String, dynamic> toJson() => {
    'imageId': id,
    'url': url,
  };
}