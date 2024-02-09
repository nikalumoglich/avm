terraform init
terraform apply -auto-approve

aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin 951726806028.dkr.ecr.us-east-1.amazonaws.com

./docker/docker-build.sh

docker tag avm-backend:latest 951726806028.dkr.ecr.us-east-1.amazonaws.com/avm:latest

docker push 951726806028.dkr.ecr.us-east-1.amazonaws.com/avm:latest

