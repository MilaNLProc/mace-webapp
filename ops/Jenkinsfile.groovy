def image

pipeline {
  agent {
    label '1.1.2'
  }
  environment {
    DOCKER_REPOSITORY = '182581879643.dkr.ecr.us-east-1.amazonaws.com/bap-datascience-r-mvp'
    DOCKER_BUILDKIT = 1
  }
  stages {
    stage('Build') {
      steps {
        script {
          image = docker.build("${DOCKER_REPOSITORY}:${GIT_COMMIT}", '--target main .')
        }
      }
    }
    stage('Test') {
      steps {
        script {
          docker.build("${DOCKER_REPOSITORY}:${GIT_COMMIT}", '--target test .')
        }
      }
    }
    stage('Push') {
      steps {
        script {
          image.push()
        }
      }
    }
    stage('Deploy') {
      steps {
        withCredentials([
          file(credentialsId: 'kubeconfig', variable: 'KUBE_CONFIG')
        ]) {
          writeFile(
            file: 'values.yaml',
            text: """
              image:
                repository: "${DOCKER_REPOSITORY}"
                tag: "${GIT_COMMIT}"
              ingress:
                enabled: true
                hosts:
                - host: r-starter-mvp.bap.aagdev.bain.com
                  paths: ['/']
            """
          )
          /*
          Helm initial installation command:
            helm install datascience-r-mvp bain/datascience-r-mvp \
              --version 0.0.1 \
              --namespace starter-projects
          */
          sh """
            helm3 upgrade \
              datascience-r-mvp bain/datascience-r-mvp \
              --values values.yaml \
              --kubeconfig ${KUBE_CONFIG} \
              --kube-context bap-dev \
              --namespace starter-projects \
              --wait
          """
        }
      }
    }
  }
}
