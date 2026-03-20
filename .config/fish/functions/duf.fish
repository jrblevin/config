function duf --wraps=du\ -sk\ \*\ \|\ sort\ -nr\ \|\ perl\ -ne\ \'\(\$s,\$f\)=split\(m\{\\t\}\)\;for\ \(qw\(K\ M\ G\)\)\ \{if\(\$s\<1024\)\ \{printf\(\"\%.1f\",\$s\)\;print\ \"\$_\\t\$f\"\;\ last\}\;\$s=\$s/1024\}\' --description alias\ duf\ du\ -sk\ \*\ \|\ sort\ -nr\ \|\ perl\ -ne\ \'\(\$s,\$f\)=split\(m\{\\t\}\)\;for\ \(qw\(K\ M\ G\)\)\ \{if\(\$s\<1024\)\ \{printf\(\"\%.1f\",\$s\)\;print\ \"\$_\\t\$f\"\;\ last\}\;\$s=\$s/1024\}\'
  du -sk * | sort -nr | perl -ne '($s,$f)=split(m{\t});for (qw(K M G)) {if($s<1024) {printf("%.1f",$s);print "$_\t$f"; last};$s=$s/1024}' $argv
        
end
