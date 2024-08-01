terraform init
terraform apply -auto-approve

aws ecr get-login-password --region sa-east-1 | docker login --username AWS --password-stdin 951726806028.dkr.ecr.sa-east-1.amazonaws.com

./docker/docker-build.sh

docker tag avm-backend:latest 951726806028.dkr.ecr.sa-east-1.amazonaws.com/avm:latest

docker push 951726806028.dkr.ecr.sa-east-1.amazonaws.com/avm:latest

aws eks update-kubeconfig --region sa-east-1 --name avm

kubectl delete pods avm-backend

# kubectl run avm-backend --image=951726806028.dkr.ecr.sa-east-1.amazonaws.com/avm:latest --port=3000 --restart=Never

kubectl create namespace avm

kubectl apply -f kube_deployment.yml

kubectl apply -f kube_lb.yml

kubectl expose deployment avm-backend  --type=ClusterIP  --name=avm-backend-cluster-ip


kubectl apply -f https://s3.us-west-2.amazonaws.com/amazon-eks/docs/eks-console-full-access.yaml

eksctl create iamidentitymapping \
    --cluster avm \
    --region=sa-east-1 \
    --arn arn:aws:iam::951726806028:role/avm-eks \
    --group avm \
    --no-duplicate-arns

kubectl create namespace aws-observability

kubectl label namespaces aws-observability aws-observability=enabled

kubectl apply -f aws-logging-cloudwatch-configmap.yaml

kubectl patch deployment coredns \
      --namespace kube-system \
      --type=json -p='[{"op": "remove", "path": "/spec/template/metadata/annotations", "value": "eks.amazonaws.com/compute-type"}]' \

kubectl rollout restart -n kube-system deployment coredns