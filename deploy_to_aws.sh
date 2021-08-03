echo 'Starting to Deploy...'
ssh ubuntu@ubuntu@3.234.99.37 " sudo docker image prune -f 
        cd /newstorage/users/sujay/R_check 
        sudo docker-compose down
        git fetch origin
        git reset --hard origin  &&  echo 'You are doing well'
        sudo docker-compose build && sudo docker-compose up -d
        "
echo 'Deployment completed successfully'
